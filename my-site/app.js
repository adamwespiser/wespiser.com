var express = require('express');
var app = express();

app.use(express.static('/home/adamwespiser/wespiser.com/my-site'));
app.listen(3001);
