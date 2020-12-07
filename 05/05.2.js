const fs = require("fs");

fs
   .readFileSync("words.txt")
   .split("\n")
   .filter(word => {
      word.replace(/(.)\1{2}/g, "$1$1")
   });
