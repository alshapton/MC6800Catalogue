def getDateRangeFromWeek(p_year,p_week):
    import datetime

    firstdayofweek = datetime.datetime.strptime(f'{p_year}-W{int(p_week )- 1}-1', "%Y-W%W-%w").date()
    lastdayofweek = firstdayofweek + datetime.timedelta(days=6.9)
    #return firstdayofweek, lastdayofweek
    return firstdayofweek.strftime("%d-%b-%Y").upper(),lastdayofweek.strftime("%d-%b-%Y").upper()
