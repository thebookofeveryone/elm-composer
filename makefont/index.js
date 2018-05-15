#!/usr/bin/env node

const minimist = require("minimist");
const opentype = require("opentype.js");
const path = require("path");
const util = require("util");

main(minimist(process.argv.slice(2)));

async function main(argv) {
  if (argv._.length !== 1) {
    console.log("Show Help");
    process.exit(-1);
  }
  if (!argv.enc) {
    console.log("an encoding (codepage) is required");
    process.exit(-1);
  }
  const filepath = argv._[0];
  const encodingFilepath = argv.enc;
  const loadFont = util.promisify(opentype.load);
  const font = await loadFont(filepath);
  const k = 1000 / font.unitsPerEm;
  const sCapHeight = font.tables.os2.sCapHeight;
  const ascent = Math.round(k * font.ascender);

  const obj = {
    Tp: path.extname(filepath) == ".ttf" ? "TrueType" : "OpenType",
    Name: font.names.fontFamily.en,
    Desc: {
      // To obtain same values than gofpdf use font.tables.os2.sTypoAscender
      // and font.tables.os2.sTypoDescender.
      Ascent: ascent,
      Descent: Math.round(k * font.descender),
      // https://github.com/jung-kurt/gofpdf/blob/master/font.go#L293
      CapHeight: sCapHeight ? Math.round(k * sCapHeight) : ascent,
      Flags: buildFlags(font),
      FontBBox: {
        Xmin: Math.round(k * font.tables.head.xMin),
        Ymin: Math.round(k * font.tables.head.yMin),
        Xmax: Math.round(k * font.tables.head.xMax),
        Ymax: Math.round(k * font.tables.head.yMax)
      },
      ItalicAngle: font.tables.post.italicAngle,
      StemV: buildStemV(font),
      MissingWidth: Math.round(k * font.tables.os2.xAvgCharWidth),
      Up: Math.round(k * font.tables.post.underlinePosition),
      Ut: Math.round(k * font.tables.post.underlineThickness)
    },
    Enc: buildEncodingName(encodingFilepath),
    Diff: "", // Not Implemented
    File: "", // Not Implemented
    Size1: 0, // Not Implemented
    Size2: 0, // Not Implemented
    OriginalSize: 0, // Not Implemented
    I: 0, // Not Implemented
    N: 0, // Not Implemented
    DiffN: 0 // Not Implemented
  };

  console.log(obj);
}

async function loadCodePage(filepath) {
  const readFile = util.promisify(fs.readFile);
  file = await readFile(filepath);
  console.log(file);
}

function buildEncodingName(filepath) {
  const basename = path.basename(filepath);
  const ext = path.extname(basename);
  return path.basename(basename, ext);
}

function buildFlags(font) {
  // https://github.com/jung-kurt/gofpdf/blob/master/font.go#L295
  let flags = 1 << 5;
  if (font.tables.post.isFixedPitch) {
    flags = flags | 1;
  }
  if (font.tables.post.italicAngle != 0) {
    flags = flags | (1 << 6);
  }
  return flags;
}

function buildStemV(font) {
  // https://github.com/jung-kurt/gofpdf/blob/master/font.go#L302
  const weight = font.tables.os2.usWeightClass;
  if (weight > 500) {
    return 120;
  } else {
    return 70;
  }
}
