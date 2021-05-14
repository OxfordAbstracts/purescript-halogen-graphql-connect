const { generateSchema } = require('purescript-graphql-client')

module.exports = () =>
  generateSchema({
    dir: './src/generated',
    modulePath: ['Generated', 'Gql'],
    useNewtypesForRecords: false,
    url: 'http://localhost:4000/graphql'
  })
