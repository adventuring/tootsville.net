(In-Package :Oliphaunt)

;;; Time handling.

(DeFun Days-Ago (Days)
  "Return a time that is DAYS days in the past."
  (Timestamp- (Now) Days :Day))

(DeFun Yesterday ()
  "Get the same time as now, but yesterday.

See: `DAYS-AGO'"
  (Days-Ago 1))

(DeFun 2-Days-Ago ()
  "Two days ago.

See: `DAYS-AGO'"
  (Days-Ago 2))

(DeFun 3-Days-Ago ()
  "Three days ago.

See: `DAYS-AGO'"
  (Days-Ago 3))

(DeFun Header-Time (&Optional (Time (Get-Universal-Time)))
  "Format TIME (or now) as an RFC-1123 timestring for HTTP headers.

Accepts either a LOCAL-TIME:TIMESTAMP or NUMBER of Universal Time."
  (Format-RFC1123-Timestring
   Nil
   (ETypeCase Time
     (Number (Universal-To-Timestamp Time))
     (Timestamp Time))))

(DeFun Year<-Universal-Time (Time)
  (Nth-Value 5 (Decode-Universal-Time Time)))

(DeFun File-Write-Year (File)
  (Or (Year<-Universal-Time (File-Write-Date File))
      0))



(DeFun Translate-American-ish-Date (Created-At)
  (Register-Groups-Bind ((#'Parse-Integer Month)
                         (#'Parse-Integer Day)
                         (#'Parse-Integer Year)
                         nil
                         (#'Parse-Integer Hour)
                         (#'Parse-Integer Minute)
                         (#'Parse-Integer Second))
      ("(\\d\\d)/(\\d\\d)/((19|20)\\d\\d) ([012]\\d):([0-5]\\d):([0-5]\\d)"
       Created-At :SharedP T)
    (Check-Type Year (Integer 1950 2050))
    (Check-Type Month (Integer 1 12))
    (Check-Type Day (Integer 1 31))
    (Assert (<= Day (Case Month
                      ((4 6 9 11) 30)
                      (2 (Case Year
                           (2016 29)
                           (Otherwise 28)))
                      ((1 3 5 7 8 10 12) 31))))
    (Check-Type Hour (Integer 00 (24)))
    (Check-Type Minute (Integer 0 (60)))
    (Check-Type Second (Integer 0 (60)))
    (Encode-Timestamp 0 Second Minute Hour Day Month Year)))
