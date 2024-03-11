/* 
Homework 7
Name: Riley Rice
Date: 3-6-2024
CS 381 - Programming Language Funadmentals 
*/

/* course(course_number, course_name, credits) */

course(cs101,python, 2).
course(mth210, calculusI, 5).
course(cs120, web_design, 3).
course(cs200, data_structures, 4).
course(cs210, algorithms, 4).
course(wrt101, basic_writing, 3).

/* section(CRN, course_number) */

section(1522,cs101).
section(1122,cs101).
section(2322,mth210).
section(2421,cs120).
section(8522,mth210).
section(1621,cs200).
section(7822,mth210).
section(2822,cs210).
section(3522,wrt101).

/* place( CRN, building, time) */

place(1522,owen102,10).
place(1122,dear118,11).
place(2322,with210,11).
place(2421,cov216,15).
place(8522,kec1001,13).
place(1621,cov216,14).
place(7822,kec1001,14).
place(2822,owen102,13).
place(3522,with210,15).

/* enroll(sid, CRN) */

enroll(122,1522).
enroll(122,8522).
enroll(150,1522).
enroll(150,2421).
enroll(212,7822).
enroll(300,2822).
enroll(300,8522).
enroll(310,3522).
enroll(310,8522).
enroll(310,1621).
enroll(175,2822).
enroll(175,7822).
enroll(175,3522).
enroll(410,1621).
enroll(410,7822).
enroll(113,3522).

/* student(sid, student_name, major) */

student(122, mary, cs).
student(150, john, math).
student(212, jim, ece).
student(300, lee, cs).
student(310, pat, cs).
student(175, amy, math).
student(410, john, cs).
student(113, zoe, ece).

% Problem 1: College Database Application

/*
    This predicate gives the course name, building, and time of the classes a student
    is taking when given a studentID. This works by simply populating values in the
    predicate with values in our database.
*/
schedule(StudentID, CourseName, Building, Time) :-
    enroll(StudentID, CRN), 
    section(CRN, CourseNumber), 
    course(CourseNumber, CourseName, _), 
    place(CRN, Building, Time). 

/*
    This predicate is a little different then our previous predicate as it only
    takes 3 parameters. It still returns the course name, building, and time of 
    the classes a student is taking, but instead it returns the studentName also.
*/
schedule(StudentID, StudentName, Course) :-
    student(StudentID, StudentName, _), 
    findall(CourseName, (
        enroll(StudentID, CRN), 
        section(CRN, CourseNumber), 
        course(CourseNumber, CourseName, _) 
    ), Courses), 
    member(Course, Courses). 

/*
    This predicate returns the course number, different sections, and times 
    for a given course that a student is taking. This works by simply getting
    values and filling those from our database.
*/
offer(CourseNumber, CourseName, CRN, Time) :-
    course(CourseNumber, CourseName, _), 
    section(CRN, CourseNumber), 
    place(CRN, _, Time). 

/*
    This predicate returns whether a given student has a conflict in their schedule
    by checking to see if any of their classes meet at the same time.
*/
conflict(StudentID, CRN1, CRN2) :-
    % Check to see whether the students are enrolled in the classes
    enroll(StudentID, CRN1),
    enroll(StudentID, CRN2),
    CRN1 \= CRN2, % Make sure that the CRNs arent the same
    % Check if they have conflicting times
    place(CRN1, _, Time),
    place(CRN2, _, Time).

/*
    This predicate returns a list of all students who can meet in a classroom.
    There are two instances in which students can meet and that is through either
    attending the same class or by having classes back to back. For that reason in 
    this predicate we handle the two cases in which two students are enrolled in the same class
    and then the second section which looks to see if two students are enrolled in a class
    in the same classroom back to back using the time and room.
*/
meet(StudentID1, StudentID2) :- 
    % This handles the case where students are in the same class
    (enroll(StudentID1, CourseID), enroll(StudentID2, CourseID), StudentID1 \= StudentID2);
    ( % This handles back-to-back classes in the same classroom
        enroll(StudentID1, CourseID1), enroll(StudentID2, CourseID2), 
        section(CourseID1, _), section(CourseID2, _), 
        place(CourseID1, Room, Time1), place(CourseID2, Room, Time2), 
        Time1 + 1 =:= Time2
    ).

/*
    This predicate returns the roster of a course, meaning all the studens that
    are in a class. This works by first getting students that are enrolled in the 
    CRN that is passed and then using the resulting StudentID to get the correct 
    StudentName.
*/
roster(CRN, StudentName) :-
    enroll(StudentID, CRN),
    student(StudentID, StudentName, _).

/*
    This predicate returns all courses that are worth 
    4 or more credits.
*/
highCredits(CourseName) :-
    course(_, CourseName, Credits),
    Credits >= 4.

/* 
    This predicate returns the amount of classes a student is enrolled
    in by looking for the passed student and finding all the classes the 
    student is enrolled in putting that into a list. We then can take the length
    of the list as the number of classes.
*/
countClasses(StudentID, StudentName, Number) :-
    student(StudentID, StudentName, _),
    findall(CRN, enroll(StudentID, CRN), Classes),
    length(Classes, Number).

% Problem 2: Path in a weighted DAG: 

% Representation of edges in given DAG graph 
edge(a, b, 2).
edge(b, c, 6).
edge(c, e, 9).
edge(e, f, 8).
edge(a, d, 1).
edge(d, f, 7).
edge(d, c, 10).
edge(b, e, 2).
edge(a, e, 1).

/*
    Predicate to find paths from S to F, Also maps dagPaths/4 to dagPathHelper/5 
    which also holds a visited array to make sure we dont visit duplicate paths.
*/
dagPaths(S, F, Path, Cost) :-
    dagPathsHelper(S, F, [S], Path, Cost).

/*
    This handles the base case where we have reached our final desintation. It
    also handles the case where the start and finish are the same in which we 
    return a list of only that element and a cost of 0.
*/
dagPathsHelper(F, F, _, [F], 0).

/*
    This is the main traversal predicate that gets the cost of the edge
    between the start and next node, then makes sure the next node isn't in
    our visited array so that we don't have duplicate paths that we are traversing.
    We then recursively call our helper predicate with the new start node as 
    the next node and the same finish node. We also append the Next node to our
    visited list. We then redundantly pass our Path and our RestCost. Then once 
    that recursive call has returned we can use it's cost without our cost and set 
    the cost to our cost plus the recursive call's cost which once the recursion tree
    has finished will return the overall cost. One more thing that can be mentioned 
    is that the path list is constructed through every call of this function by
    concatenating the start node of every call to our path.
*/
dagPathsHelper(S, F, Visited, [S | Path], Cost) :-
    edge(S, Next, EdgeCost),
    \+ member(Next, Visited),  % Ensure no duplicate vertices in the path
    dagPathsHelper(Next, F, [Next | Visited], Path, RestCost),
    Cost is EdgeCost + RestCost.
