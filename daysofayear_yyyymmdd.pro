


pro daysofayear_yyyymmdd,yyyy,daysoftheyear

jan = long(findgen(31)) + 101l + yyyy*10000l
if yyyy eq 2012 or yyyy eq 2008 or yyyy eq 2004 $
  or yyyy eq 2016 or yyyy eq 2020 then $
  feb = long(findgen(29)) + 201l + yyyy*10000l else $
  feb = long(findgen(28)) + 201l + yyyy*10000l
mar = long(findgen(31)) + 301l + yyyy*10000l
apr = long(findgen(30)) + 401l + yyyy*10000l
may = long(findgen(31)) + 501l + yyyy*10000l
jun = long(findgen(30)) + 601l + yyyy*10000l
jul = long(findgen(31)) + 701l + yyyy*10000l
aug = long(findgen(31)) + 801l + yyyy*10000l
sep = long(findgen(30)) + 901l + yyyy*10000l
oct = long(findgen(31)) + 1001l + yyyy*10000l
nov = long(findgen(30)) + 1101l + yyyy*10000l
dec = long(findgen(31)) + 1201l + yyyy*10000l


daysoftheyear = [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec]






end
