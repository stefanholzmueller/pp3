exports.handler = (event, context, callback) => {

    var stats = {
        attributes: JSON.parse(event.queryStringParameters.attributes),
        skill: Number.parseInt(event.queryStringParameters.skill)
    }
    var difficulty = Number.parseInt(event.queryStringParameters.difficulty)
    var dice = JSON.parse(event.queryStringParameters.dice)

    var Check = require('../output/Check')
    var result = Check.evaluateToString(stats)(difficulty)(dice)

    var response = {
        "statusCode": 200,
        "headers": {},
        "body": result,
        "isBase64Encoded": false
    }
    callback(null, response)
}
