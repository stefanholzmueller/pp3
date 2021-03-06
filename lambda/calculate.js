exports.handler = (event, context, callback) => {

    var stats = {
        attributes: JSON.parse(event.queryStringParameters.attributes),
        skill: Number.parseInt(event.queryStringParameters.skill)
    }
    var difficulty = Number.parseInt(event.queryStringParameters.difficulty)

    var Check = require('../output/Check')
    var result = Check.calculate(stats)(difficulty)

    var response = {
        "statusCode": 200,
        "headers": {},
        "body": JSON.stringify(result),
        "isBase64Encoded": false
    }
    callback(null, response)
}
