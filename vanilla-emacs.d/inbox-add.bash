WORK_DIARY_ORG_PATH="$(eval echo "~/Dropbox/work-diary.org")"
# I redundantly escape < and > here to not trigger the noweb interpolation.
WORK_DIARY_ORG_INBOX_TARGET="\<\<Insert INBOX tasks here\>\>"

# Obtain URI from the clipboard, if there is one.
uri=$(wl-paste)
if [[ ! "$uri" =~ ^[a-zA-Z][a-zA-Z0-9+.-]*:// ]]; then
  uri=""
fi

SEPARATOR=";"
input=$(yad --title="Inbox" --form --separator="${SEPARATOR}" \
            --field="Title" "" \
            --field="Uri" "${uri}" \
            --field="Description" "" \
            --button=Add:0) || exit 1
title=$(echo "${input}" | cut -d"${SEPARATOR}" -f1)
uri=$(echo "${input}" | cut -d"${SEPARATOR}" -f2)
description=$(echo "${input}" | cut -d"${SEPARATOR}" -f3)

if [ -z "${title}" ]; then exit 1; fi

org_heading="** INBOX ${title}"
if [ -n "$uri" ]; then
  org_heading="$org_heading ([[${uri}][URI]])"
fi
if [ -n "$description" ]; then
  org_heading="$org_heading"$'\n'"  ${description}"
fi

sed -i "/${WORK_DIARY_ORG_INBOX_TARGET}/r /dev/stdin" "${WORK_DIARY_ORG_PATH}" <<< "${org_heading}"
