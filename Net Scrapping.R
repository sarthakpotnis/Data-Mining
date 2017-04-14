page= readLines('http://asia.nikkei.com/ ')
grep('Taiwan',page)
page[217:448]
mypattern = '<td class="row-text">([^<]*)</td>'
datalines = grep(mypattern,page[0:length(page)],value=TRUE)
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)
matches = mapply(getexpr,datalines,gg)
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result[1:10]
