
#!/bin/bash

set -u

mkdir -p data/raw
cd data/raw || exit 1

BASE_URL="https://s3.amazonaws.com/tripdata"

for year in 2024 2025; do
    for month in {01..12}; do

        date="${year}${month}"
        echo "Downloading ${date}..."

        success=0

        for suffix in \
            "-citibike-tripdata.zip" \
            "-citibike-tripdata.csv.zip"
        do
            filename="${date}${suffix}"

            # Skip existing valid files
            if [[ -f "$filename" ]] && unzip -tq "$filename" >/dev/null 2>&1; then
                echo "Already downloaded: $filename"
                success=1
                break
            fi

            # Download to a temporary file
            if wget -c --tries=3 --timeout=60 \
                "${BASE_URL}/${filename}" \
                -O "${filename}.part"; then

                if unzip -tq "${filename}.part" >/dev/null 2>&1; then
                    mv "${filename}.part" "$filename"
                    echo "Completed: $filename"
                    success=1
                    break
                fi
            fi

            rm -f "${filename}.part"
        done

        if [[ "$success" -eq 0 ]]; then
            echo "FAILED: ${date}" >&2
        fi

    done
done

echo "Download process finished."

