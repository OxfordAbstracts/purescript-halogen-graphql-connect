
const widgets = [
  { id: 1, name: 'widget one', colour: 'RED' },
  { id: 2, name: 'widget two', colour: 'GREEN' },
]

module.exports = (onListening) => {
  const express = require('express')
  const { graphqlHTTP } = require('express-graphql')
  const { buildSchema } = require('graphql')
  const cors = require('cors')

  const schema = buildSchema(`
    type Query {
        prop: String
        widgets(id: Int): [Widget!]!
    }

    type Widget {
        id: Int
        name: String!
        colour: Colour!
    }

    enum Colour {
      RED
      GREEN
      BLUE
    }

    type Mutation { 
      set_widget_colour(id: Int!, colour: Colour!): Int!
    }

    `)

  const root = {
    prop: () => 'Hello world!',
    widgets: ({ id, colour }) =>
      widgets.filter(w => (!id || id === w.id)),
    set_widget_colour: ({ id, colour }) => {
      let count = 0
      widgets.forEach((w) => {
        if (w.id === id) {
          w.colour = colour
          count++
        }
      })
      return count
    }
  }

  const app = express()
  app.use(cors())

  app.use('/graphql', graphqlHTTP({
    schema,
    rootValue: root,
    graphiql: true
  }))

  app.listen(4000, onListening)
}
