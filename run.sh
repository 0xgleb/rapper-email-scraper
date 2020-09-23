source ./twitter.env

while true; do
    stack exec get-rapper-emails
    sleep 900
done
