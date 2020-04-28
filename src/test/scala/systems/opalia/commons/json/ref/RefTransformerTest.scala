package systems.opalia.commons.json.ref

import org.scalatest._
import play.api.libs.json.Json


class RefTransformerTest
  extends FlatSpec
    with Matchers {

  it should "parse JSON pointers correctly" in {

    val documents =
      Seq(
        RefTransformer.Document("main.json", Json.parse(
          """
            |{
            |  "paths": {
            |    "/users": {
            |      "get": {
            |        "operationId": "getUsers",
            |        "responses": {
            |          "200": {"$ref": "./components/models/users.json#/components/responses/Users"}
            |        }
            |      },
            |      "post": {
            |        "operationId": "createUser",
            |        "requestBody": {"$ref": "./components/requestBodies.json#/components/requestBodies/UserCreation"},
            |        "responses": {
            |          "200": {"$ref": "./components/models/user.json#/components/responses/User"},
            |          "400": {"$ref": "./components/errors/400.json#/components/responses/BadRequest"}
            |        }
            |      }
            |    },
            |    "/user/{userId}": {
            |      "get": {
            |        "operationId": "getUser",
            |        "parameters": [
            |          {"$ref": "./components/parameters.json#/components/parameters/userId"}
            |        ],
            |        "responses": {
            |          "200": {"$ref": "./components/models/user.json#/components/responses/User"},
            |          "400": {"$ref": "./components/errors/400.json#/components/responses/BadRequest"},
            |          "404": {"$ref": "./components/errors/404.json#/components/responses/NotFound"}
            |        }
            |      }
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/parameters.json", Json.parse(
          """
            |{
            |  "components": {
            |    "parameters": {
            |      "userId": {
            |        "name": "userId",
            |        "in": "path",
            |        "required": true,
            |        "schema": {"$ref": "./schemas.json#/components/schemas/ObjectId"}
            |      }
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/requestBodies.json", Json.parse(
          """
            |{
            |  "components": {
            |    "requestBodies": {
            |      "UserCreation": {
            |        "content": {
            |          "application/json": {
            |            "schema": {"$ref": "./schemas.json#/components/schemas/UserWrite"}
            |          }
            |        },
            |        "required": true
            |      }
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/responses.json", Json.parse(
          """
            |{
            |  "components": {
            |    "responses": {
            |      "EntityList": {
            |        "$id": "EntityList",
            |        "content": {
            |          "application/json": {
            |            "schema": {
            |              "type": "object",
            |              "properties": {
            |                "entries": {
            |                  "type": "array",
            |                  "items": {"$ref": "#/components/schemas/EntityRead"}
            |                }
            |              }
            |            }
            |          }
            |        }
            |      },
            |      "Entity": {
            |        "$id": "Entity",
            |        "content": {
            |          "application/json": {
            |            "schema": {"$ref": "#/components/schemas/EntityRead"}
            |          }
            |        }
            |      },
            |      "HttpStatus": {
            |        "$id": "HttpStatus",
            |        "content": {
            |          "application/json": {
            |            "schema": {"$ref": "./schemas/templateSchemas.json#templateSchemas/HttpStatusEntity"}
            |          }
            |        }
            |      }
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/models/users.json", Json.parse(
          """
            |{
            |  "components": {
            |    "responses": {
            |      "Users": {"$ref": "../responses.json#EntityList"}
            |    },
            |    "schemas": {
            |       "EntityRead": {"$ref": "../schemas.json#/components/schemas/UserRead"}
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/models/user.json", Json.parse(
          """
            |{
            |  "components": {
            |    "responses": {
            |      "User": {"$ref": "../responses.json#Entity"}
            |    },
            |    "schemas": {
            |       "EntityRead": {"$ref": "../schemas.json#/components/schemas/UserRead"}
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/errors/400.json", Json.parse(
          """
            |{
            |  "components": {
            |    "responses": {
            |      "BadRequest": {"$ref": "../responses.json#HttpStatus"}
            |    },
            |    "examples": {
            |      "StatusCode": 400,
            |      "StatusName": "BadRequestError",
            |      "StatusMessage": "Malformed/invalid parameters or request body."
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/errors/404.json", Json.parse(
          """
            |{
            |  "components": {
            |    "responses": {
            |      "NotFound": {"$ref": "../responses.json#HttpStatus"}
            |    },
            |    "examples": {
            |      "StatusCode": 404,
            |      "StatusName": "NotFoundError",
            |      "StatusMessage": "Entity not found."
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/schemas.json", Json.parse(
          """
            |{
            |  "components": {
            |    "schemas": {
            |      "UserRead": {
            |        "type": "object",
            |        "properties": {
            |          "id": {"$ref": "#/components/schemas/ObjectId"},
            |          "firstName": {
            |            "type": "string"
            |          },
            |          "lastName": {
            |            "type": "string"
            |          }
            |        }
            |      },
            |      "UserWrite": {
            |        "type": "object",
            |        "properties": {
            |          "email": {
            |            "type": "string"
            |          },
            |          "firstName": {
            |            "type": "string"
            |          },
            |          "lastName": {
            |            "type": "string"
            |          }
            |        }
            |      },
            |      "ObjectId": {
            |        "type": "string",
            |        "format": "object-id",
            |        "pattern": "^[0-9a-f]{24}$"
            |      }
            |    }
            |  }
            |}
      """.stripMargin.trim)),
        RefTransformer.Document("components/schemas/templateSchemas.json", Json.parse(
          """
            |{
            |  "components": {
            |    "schemas": {
            |      "$id": "templateSchemas",
            |      "HttpStatusEntity": {
            |        "type": "object",
            |        "properties": {
            |          "code": {
            |            "type": "integer",
            |            "example": {"$ref": "#/components/examples/StatusCode"}
            |          },
            |          "name": {
            |            "type": "string",
            |            "example": {"$ref": "#/components/examples/StatusName"}
            |          },
            |          "message": {
            |            "oneOf": [
            |              {
            |                "type": "string"
            |              },
            |              {
            |                "type": "array",
            |                "items": {
            |                  "type": "string"
            |                }
            |              }
            |            ],
            |            "example": {"$ref": "#/components/examples/StatusMessage"}
            |          }
            |        }
            |      }
            |    }
            |  }
            |}
      """.stripMargin.trim))
      )

    val transformer =
      RefTransformer(new Resolver {

        def resolve(currentUri: String, previousUri: String): String =
          DefaultResolver.resolve(currentUri, previousUri)

        def load(uri: String): RefTransformer.Document =
          documents.find(_.uri == uri).getOrElse(throw new IllegalArgumentException(s"Cannot load uri: $uri"))
      })

    transformer.transform("main.json")
  }
}
