-- List of ones
ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]

fileList = ["file1.txt","file2.txt","file3.txt"
                     ,"file4.txt","file5.txt","file6.txt","file7.txt"
                     ,"file8.txt"]

students = ["Bob","Kathy","Sue","Joan","Jim","Mike"]
