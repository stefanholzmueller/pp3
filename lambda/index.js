exports.handler = (event, context, callback) => {

    var stats = {
        attributes: JSON.parse(event.queryStringParameters.attributes),
        skill: Number.parseInt(event.queryStringParameters.skill)
    }
    var difficulty = Number.parseInt(event.queryStringParameters.difficulty)
    var dice = JSON.parse(event.queryStringParameters.dice)

    var Check = require('./Check')
    var result = Check.evaluateToString(stats)(difficulty)(dice)

    var response = {
        "statusCode": 200,
        "headers": {},
        "body": result,
        "isBase64Encoded": false
    }
    callback(null, response)
}

/*
    console.log("a")
    var event = {
        queryStringParameters: {
            attributes: "[11,12,13]",
            dice: "[1,2,3]",
            difficulty: "3",
            skill: "7"
        }
    }
    exports.handler(event, null, (error, response) => console.log(response))
*/