## IMAP/S

- `fetch(uid as Number)` as `Message`

- `fetch(uid as Number, headers[] as Text)` as `Message` # contains only headers requested

- `list(folder as Text)` as `Number[]` list messages in folder <- lists UIDs? why not some kind of metadata for those?


Start with
```javascript
var sess = IMAP {
    username = "xx",
    password = "xx", // or xoauth2 = "asdasdasd" but not both
    server = "htsdaer.net",
    port = 993
}
```

Then you can do
```javascript
var folders[] as String = list(sess) // gives a list of folders
var fold1 = folders[1]
var flags = flags(sess, folder=fold1) // call flags on a folder to get its flags
var msgIDs[] as Number = list(sess, folder=fold1)
var msgflags = flags(sess, folder=fold1, msgID=msgIDs[1]) // flags of the message
var msg as MailMessage = fetch(sess, folder=fold1, msgID=msgIDs[1]) // fetch whole message
var hd[String] as String = headers(sess, folder=fold1, msgID=msgIDs[1]) // headers only
var hdx as String = headers(sess, folder=fold1, msgID=msgIDs[1], field="Content-Type") //specific header
var msgs[] as MailMessage =
    imap.fetch(sess, folder=fold1, msgIDs=msgIDs[3:end]) // multiple messages
OR  imap.fetch(sess, folder=fold1)  // ALL messages in folder
// this func handles multithreading automatically and goes zip zap zoom
// but if you want a callback:
event sess.fetchDone = myfunc // Function(imap.Message)()
fetch(...) // will call myfunc with a new msg everytime a fetch is done

MailMessage:

add(msg as MailMessage, part as MIMEPart)
attach(msg as MailMessage, file as String)

```


// btw how about ctors receive a Dict in jet?

## Email

Contains functions for keeping state of an email and encoding/MIME functions. Or should MIME functions be separate anyway?

flags:
`.flagged`, `.read` (seen), `.clean` (not junk)

## POP3/S

## SMTP/S

## HTTP/S

## FTP/S

## SSH

## SCP/SFTP

All functions must have a way to ignore output (just check headers) esp. HTTP