const exec = require('exec-sh').promise

const go = () => {
  require('./server-fn')(async () => {
    console.info('Running a GraphQL API server at http://localhost:4000/graphql')
    await require('./generate-purs-schema')()
    console.info('Schema built')
    await exec('npm run build', { stdio: 'pipe', stderr: 'pipe' })
    console.info('Purs built')
    await exec('npm run serve-client')
  })
}

go()
