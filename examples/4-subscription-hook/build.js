const { build } = require('esbuild');


const go = async () => {
    const builder = await build({
        // Bundles JavaScript.
        bundle: true,
        // Defines env variables for bundled JavaScript; here `process.env.NODE_ENV`
        // is propagated with a fallback.
        // Bundles JavaScript from (see `outfile`).
        entryPoints: ['dev/index.js'],
        outfile: 'dev/bundle.js',
        minify: false,
        // Bundles JavaScript to (see `entryPoints`).
        write: false
    });

    // await builder.rebuild()

    console.log('built')
};

go();