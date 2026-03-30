import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";
import fs from 'fs';

const config = JSON.parse(fs.readFileSync('./src/main/resources/config.json', 'utf-8'));

export default defineConfig({
  plugins: [scalaJSPlugin()],
  base: config.base,
});