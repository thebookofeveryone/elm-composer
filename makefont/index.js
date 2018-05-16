#!/usr/bin/env node

/*
   A small command line utility for building font definitions for elm-composer.

   elm-composer cannot read font files (like TrueType or OpenType) directly,
   instead, a custom JSON description is needed to layout text.

   The format of this JSON file is similar to the jung-kurt/gofpdf font
   description (https://github.com/jung-kurt/gofpdf).

   Usage: index.js --enc path/to/encoding.map path/to/font.ttf
 */

const fs = require("fs");
const minimist = require("minimist");
const opentype = require("opentype.js");
const path = require("path");
const util = require("util");

const loadFont = util.promisify(opentype.load);
const readFile = util.promisify(fs.readFile);
const writeFile = util.promisify(fs.writeFile);

main(minimist(process.argv.slice(2)));

async function main(argv) {
  if (argv._.length !== 1) {
    console.log("usage: index.js --enc path/to/encoding.map path/to/font.ttf");
    process.exit(-1);
  }
  if (!argv.enc) {
    console.log("an encoding (codepage) is required");
    process.exit(-1);
  }
  const filepath = argv._[0];
  const encodingFilepath = argv.enc;
  const codePage = await loadCodePage(encodingFilepath);
  const codePageLength = Object.keys(codePage).length;
  const font = await loadFont(filepath);
  const k = 1000 / font.unitsPerEm;
  const sCapHeight = font.tables.os2.sCapHeight;
  const ascent = Math.round(k * font.ascender);
  const missingWidth = Math.round(k * font.tables.os2.xAvgCharWidth);

  const widths = new Array(codePageLength);
  for (const char in codePage) {
    let glyph = font.charToGlyph(char);
    const glyphWidth = glyph
      ? Math.round(k * glyph.advanceWidth)
      : missingWidth;
    widths[codePage[char].index] = glyphWidth;
  }
  for (let i = 0; i < codePageLength; i++) {
    widths[i] = widths[i] || missingWidth;
  }

  const kerns = {};
  for (const leftChar in codePage) {
    const leftGlyph = font.charToGlyph(leftChar);
    if (!leftGlyph) {
      continue;
    }
    const kernsLine = [];
    for (const rightChar in codePage) {
      const rightGlyph = font.charToGlyph(rightChar);
      if (!rightGlyph) {
        continue;
      }
      const kerning = font.getKerningValue(leftGlyph, rightGlyph);
      if (kerning !== 0) {
        kernsLine.push(codePage[rightChar].index);
        kernsLine.push(Math.round(k * kerning));
      }
    }
    if (kernsLine.length > 0) {
      kerns[codePage[leftChar].index] = kernsLine;
    }
  }

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
      MissingWidth: missingWidth
    },
    Up: Math.round(k * font.tables.post.underlinePosition),
    Ut: Math.round(k * font.tables.post.underlineThickness),
    Cw: widths,
    Ck: kerns,
    Enc: basename(encodingFilepath),
    Diff: "", // Not Implemented
    File: "", // Not Implemented
    Size1: 0, // Not Implemented
    Size2: 0, // Not Implemented
    OriginalSize: 0, // Not Implemented
    I: 0, // Not Implemented
    N: 0, // Not Implemented
    DiffN: 0 // Not Implemented
  };

  await writeFile(`${basename(filepath)}.json`, JSON.stringify(obj));
}

async function loadCodePage(filepath) {
  const file = await readFile(filepath);
  const lineList = file.toString().split("\n");
  const codePage = {};
  for (const line of lineList) {
    const wordList = line.split(" ");
    if (wordList.length !== 3) {
      continue;
    }

    const indexString = wordList[0].trim();
    if (indexString[0] !== "!") {
      continue;
    }
    const index = parseInt(indexString.slice(1), 16);

    const codepointString = wordList[1].trim();
    if (!codepointString.startsWith("U+")) {
      continue;
    }
    const codepoint = String.fromCharCode(
      parseInt(codepointString.slice(2), 16)
    );

    const name = wordList[2].trim();
    codePage[codepoint] = { index, name };
  }
  return codePage;
}

function basename(filepath) {
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
