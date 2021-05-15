const {generateSW} = require("workbox-build");
const path = require("path");

const cacheId = "keck-mb-parts-exchange-manager";
const distDir = path.join(process.cwd(), "dist");
const swDist = path.join(distDir, "sw.js");

(async () => {
    const result = await generateSW({
        cacheId: cacheId,
        swDest: swDist,
        globDirectory: "./dist/",
        globPatterns: ["**/*.{html,js,css}"],
        runtimeCaching: []
    });
    for (const warning of result.warnings) {
        console.warn(warning);
    }
    console.log(
        `Generated ${swDist}, which will precache ${result.count} files, totaling ${result.size} bytes.`
    );
})();