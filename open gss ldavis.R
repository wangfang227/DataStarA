
ldatopic(4)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_4"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

ldatopic(6)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_6"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

ldatopic(10)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_10"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

set.seed(1234)
ldatopic(25)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_25"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)

ldatopic(100)
#open html
out.dir="C:\\Work\\gssdata\\LDAvis\\GSS_100"
servd <- suppressMessages(suppressWarnings(require('servr')))
httd(dir = out.dir)



