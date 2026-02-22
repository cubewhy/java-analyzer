import * as vscode from "vscode";
import { ExtensionContext } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverPath = getServerPath(context);

  const serverOptions: ServerOptions = {
    run: { command: serverPath },
    debug: { command: serverPath, args: [] },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "java" },
      { scheme: "file", language: "kotlin" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  client = new LanguageClient(
    "java-analyzer",
    "java-analyzer",
    serverOptions,
    clientOptions,
  );

  client.start();
}

function getServerPath(context: vscode.ExtensionContext): string {
  const devServerPath = process.env.SERVER_PATH;
  if (devServerPath) {
    return devServerPath;
  }

  const configPath = vscode.workspace
    .getConfiguration("myLsp")
    .get<string>("serverPath");
  if (configPath) {
    return configPath;
  }

  const extName = process.platform === "win32" ? ".exe" : "";
  const bundledPath = vscode.Uri.joinPath(
    context.extensionUri,
    "bin",
    `server${extName}`,
  ).fsPath;

  return bundledPath;
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
