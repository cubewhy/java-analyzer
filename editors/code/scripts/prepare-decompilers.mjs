import { cpSync, existsSync, mkdirSync, readdirSync, rmSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const CFR_BUILDER = "maven:3.9.12-eclipse-temurin-8"; // cfr requires java 6 target
const VINEFLOWER_BUILDER = "eclipse-temurin:17-jdk";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const extensionRoot = resolve(__dirname, "..");
const repoRoot = resolve(extensionRoot, "..", "..");
const resourcesDir = join(extensionRoot, "resources", "decompilers");

const cfrRoot = join(repoRoot, "third_party", "cfr");
const vineflowerRoot = join(repoRoot, "third_party", "vineflower");

function runCommand(command, args, cwd) {
  const result = spawnSync(command, args, {
    cwd,
    stdio: "inherit",
    shell: process.platform === "win32",
  });

  if (result.status !== 0) {
    throw new Error(`Command failed: ${command} ${args.join(" ")}`);
  }
}

function runDocker(image, workdirInRepo, shellCommand, extraEnv = []) {
  const args = [
    "run",
    "--rm",
    "-v",
    `${repoRoot}:/workspace:Z`,
    "-w",
    `/workspace/${workdirInRepo}`,
  ];

  for (const [key, value] of extraEnv) {
    args.push("-e", `${key}=${value}`);
  }

  args.push(image, "bash", "-lc", shellCommand);
  runCommand("docker", args, repoRoot);
}

function findNewestJar(dir, predicate) {
  if (!existsSync(dir)) {
    return undefined;
  }

  const jars = readdirSync(dir)
    .filter((name) => name.endsWith(".jar"))
    .filter(predicate)
    .map((name) => join(dir, name));

  if (jars.length === 0) {
    return undefined;
  }

  return jars.sort((a, b) => b.localeCompare(a))[0];
}

function ensureDockerAvailable() {
  const check = spawnSync("docker", ["--version"], {
    cwd: repoRoot,
    stdio: "ignore",
    shell: process.platform === "win32",
  });

  if (check.status !== 0) {
    throw new Error(
      "Docker is required to bundle decompilers. Install Docker and re-run `pnpm run prepare:decompilers`.",
    );
  }
}

function prepareCfr() {
  console.log("[decompilers] Building CFR jar in Docker...");
  runDocker(CFR_BUILDER, "third_party/cfr", "mvn -q -DskipTests package");

  const cfrJar = findNewestJar(
    join(cfrRoot, "target"),
    (name) => !name.includes("-sources") && !name.includes("-javadoc"),
  );
  if (!cfrJar) {
    throw new Error("Unable to locate built CFR jar in third_party/cfr/target");
  }

  cpSync(cfrJar, join(resourcesDir, "cfr.jar"));
}

function prepareVineflower() {
  console.log("[decompilers] Building Vineflower jar in Docker...");
  runDocker(
    VINEFLOWER_BUILDER,
    "third_party/vineflower",
    "./gradlew --no-daemon allJar",
    [["GRADLE_USER_HOME", "/tmp/gradle-home"]],
  );

  const vineflowerJar = findNewestJar(
    join(vineflowerRoot, "build", "libs"),
    (name) =>
      !name.includes("-sources") &&
      !name.includes("-javadoc") &&
      !name.includes("-slim"),
  );
  if (!vineflowerJar) {
    throw new Error(
      "Unable to locate built Vineflower jar in third_party/vineflower/build/libs",
    );
  }

  cpSync(vineflowerJar, join(resourcesDir, "vineflower.jar"));
}

function main() {
  ensureDockerAvailable();
  rmSync(resourcesDir, { recursive: true, force: true });
  mkdirSync(resourcesDir, { recursive: true });

  prepareCfr();
  prepareVineflower();
  console.log(
    "[decompilers] Bundled jars written to editors/code/resources/decompilers",
  );
}

main();
