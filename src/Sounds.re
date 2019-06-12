open Types;

let green =
  Audio.createAudio("https://s3.amazonaws.com/freecodecamp/simonSound1.mp3");

let red =
  Audio.createAudio("https://s3.amazonaws.com/freecodecamp/simonSound2.mp3");

let blue =
  Audio.createAudio("https://s3.amazonaws.com/freecodecamp/simonSound3.mp3");

let yellow =
  Audio.createAudio("https://s3.amazonaws.com/freecodecamp/simonSound4.mp3");

let error =
  Audio.createAudio(
    "https://s3.amazonaws.com/adam-recvlohe-sounds/error.wav",
  );

let force = Audio.createAudio("/public/quotes/force.mp3");
let force2 = Audio.createAudio("/public/quotes/force2.mp3");
let badfeeling = Audio.createAudio("/public/quotes/badfeeling.mp3");
let luck = Audio.createAudio("/public/quotes/luck.mp3");
let notout = Audio.createAudio("/public/quotes/notout.mp3");
let strong = Audio.createAudio("/public/quotes/strong.mp3");

let map = [(Green, green), (Red, red), (Blue, blue), (Yellow, yellow)];