import * as vscode from "vscode";

import {
  LanguageClient,
  type LanguageClientOptions,
} from "vscode-languageclient/node";

import {
  EXTENSION_ID,
  normalizeOptionalPath,
  type ExtensionSettings,
} from "./config";
import { resolveEffectiveDecompilerPath } from "./decompilerPath";
import { resolveServerOptions } from "./serverPath";

function createClientOptions(
  context: vscode.ExtensionContext,
  settings: ExtensionSettings,
  outputChannel: vscode.OutputChannel,
  fileWatcher: vscode.FileSystemWatcher,
): LanguageClientOptions {
  return {
    documentSelector: [
      { scheme: "file", language: "java" },
      { scheme: "file", language: "kotlin" },
    ],
    synchronize: {
      fileEvents: fileWatcher,
    },
    initializationOptions: {
      jdkPath: normalizeOptionalPath(settings.jdkPath),
      decompilerPath: resolveEffectiveDecompilerPath(context, settings),
      decompilerBackend: settings.decompilerBackend,
    },

    outputChannel,
  };
}

export class LanguageClientManager implements vscode.Disposable {
  private client: LanguageClient | undefined;
  private readonly outputChannel: vscode.OutputChannel;
  private readonly fileWatcher: vscode.FileSystemWatcher;
  private restartChain: Promise<void> = Promise.resolve();

  constructor(private readonly context: vscode.ExtensionContext) {
    this.outputChannel = vscode.window.createOutputChannel(EXTENSION_ID);
    this.fileWatcher = vscode.workspace.createFileSystemWatcher("**/.clientrc");
  }

  async start(settings: ExtensionSettings): Promise<void> {
    if (this.client) {
      return;
    }

    const client = new LanguageClient(
      EXTENSION_ID,
      EXTENSION_ID,
      resolveServerOptions(this.context, settings),
      createClientOptions(
        this.context,
        settings,
        this.outputChannel,
        this.fileWatcher,
      ),
    );

    this.client = client;
    await client.start();
  }

  async stop(): Promise<void> {
    const client = this.client;
    this.client = undefined;

    if (!client) {
      return;
    }

    await client.stop();

    client.dispose();
  }

  restart(settings: ExtensionSettings): Promise<void> {
    this.restartChain = this.restartChain.then(async () => {
      await this.stop();
      await this.start(settings);
    });

    return this.restartChain;
  }

  dispose(): void {
    void this.stop();
    this.fileWatcher.dispose();
    this.outputChannel.dispose();
  }
}