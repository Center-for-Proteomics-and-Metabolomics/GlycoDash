#!/usr/bin/env bash

cd "$(dirname "$0")" || exit 1

mkdir -p glycans_pdf

shopt -s nullglob
svg_files=(glycans_svg/*.svg)

if [ ${#svg_files[@]} -eq 0 ]; then
    echo "ERROR: No SVG files found in glycans_svg/"
    echo
    read -p "Press enter to close..."
    exit 1
fi

for f in "${svg_files[@]}"; do
    out="glycans_pdf/$(basename "${f%.svg}").pdf"
    echo "Converting: $f -> $out"
    rsvg-convert -f pdf -o "$out" "$f"
done

echo
echo "Finished converting SVG files to PDF."
echo "Output files are in glycans_pdf/"
echo

read -p "Press enter to close..."