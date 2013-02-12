#   Exercises

OTP Erlang exercises.

#   Testing

##  8.1 A gen_server Database Server with transactions (db_gen_server.erl)
    1> c(my_db_TEST).
    {ok,my_db_TEST}
    2> my_db_TEST:all(db_gen_server).
    Running start_stop - ok
    Running insert_read_all 1 2 3 4 - ok
    Running insert_par_read_all 4 5 3 2 6 1 7 8 9 10 11 12 13 14 15 16 17 18 22 19 20 21 23 24 25 26 27 28 29 31 32 33 30 34 35 100 40 39 38 99 37 36 41 98 42 97 43 44 96 95 45 46 47 48 49 94 50 93 92 51 52 53 54 88 55 87 56 86 85 57 58 59 83 60 61 80 62 78 91 90 89 76 84 82 81 79 77 75 74 73 64 65 63 72 66 70 69 71 68 67 - ok
    Running insert_par_overwrite_all 2 1 3 4 6 7 5 8 10 9 11 12 13 14 15 16 17 18 19 20 21 100 22 99 24 98 25 26 23 97 27 28 30 31 29 33 34 35 95 36 94 96 37 93 32 38 92 39 90 40 41 91 89 42 88 87 43 86 44 85 84 46 82 83 45 47 81 80 48 79 49 78 77 50 76 75 51 74 52 73 53 71 70 54 55 56 68 67 57 66 65 58 59 63 64 62 61 60 69 72 - ok
    ok

![Screenshot of first test]
(https://raw.github.com/pinne/erlang/master/OTP/test-8.1.png "db_gen_server")

##  8.2 A Database Server Supervisor (db_sup.erl)
    1> c(db_sup).
    {ok,db_sup}
    2> db_sup:start_link().
    {ok,<0.39.0>}
    3> whereis(db_gen_server).
    <0.40.0>
    4> exit(whereis(db_gen_server), kill).
    true
    5> whereis(db_gen_server).
    <0.48.0>
    6> db_gen_server:write(simon, berlin).
    ok
    7> db_gen_server:write(davve, sthlm).
    ok
    8> db_gen_server:match(sthlm).
    [davve]
    9> exit(whereis(db_gen_server), kill).
    true
    10> db_gen_server:match(sthlm).
    [davve]
    11> 

![Screenshot of second test]
(https://raw.github.com/pinne/erlang/master/OTP/test-8.2.png "db_sup")

##  Exercise 8.3 Database Handling using DETS (db_dets.erl)
    1> c(db_dets_TEST).
    {ok,db_dets_TEST}
    2> db_dets_TEST:all().
    Running insert_read_all 1 2 3 4 - ok
    Running insert_delete_one 1 2 3 4 - ok
    Running insert_delete_all 1 2 3 - ok
    Running insert_overwrite_one 1 2 3 - ok
    Running insert_overwrite_all 1 2 3 4 - ok
    ok

![Screenshot of third test]
(https://raw.github.com/pinne/erlang/master/OTP/test-8.3.png "db_dets")




