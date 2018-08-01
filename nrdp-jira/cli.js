#!/usr/bin/env node

const http = require('http');

var req = http.get("http://localhost:58910", resp => {
    console.log(resp);
    console.log(error);
});

req.on('error', error => {
    // console.log("Got error", error);
    if (error.code == 'ECONNREFUSED') {

    } else {
        console.log("Unknown error", error);
    }
});


