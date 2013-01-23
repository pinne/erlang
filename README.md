#   Exercises

Basic Erlang exercises.

#   Testing

##  2.4 Database handling using lists (db.erl)
    Running insert_read_all 1 2 3 4 - ok
    Running insert_delete_one 1 2 3 4 - ok
    Running insert_delete_all 1 2 3 - ok
    Running insert_overwrite_one 1 2 3 - ok
    Running insert_overwrite_all 1 2 3 4 - ok

##  4.1 A database server (my_db.erl)
    Running start_stop - ok
    Running insert_read_all 1 2 3 4 - ok
    Running insert_par_read_all 9 23 30 29 31 28 27 26 32 54 53 52 51 50 48 55 58 49 56 61 60 62 64 65 69 70 68 63 71 66 46 45 47 79 76 78 77 94 59 89 67 91 92 75 95 90 57 13 44 11 10 12 7 8 6 5 2 1 38 98 97 33 93 36 88 40 42 87 85 81 4 74 3 24 22 21 20 19 18 17 16 15 14 72 25 34 35 37 39 41 43 73 80 82 86 96 99 83 84 100 - ok
    Running insert_par_overwrite_all 19 18 17 20 2 3 4 5 6 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 16 44 45 15 46 14 95 13 92 12 11 90 10 88 9 8 7 1 85 83 81 47 82 84 100 96 99 48 98 49 97 50 94 93 51 91 52 89 53 70 74 54 69 55 56 57 21 58 22 23 59 24 60 26 61 25 66 62 65 63 64 87 86 68 80 67 79 76 72 73 71 75 78 77 - ok

##  5.3 A database server with transactions (my_db_trans.erl)
    2> my_db_trans_TEST:all(my_db_trans).
    Running simple_locking - ok
    Running unlocked_update - 7
    Running locked_update - 100

##  5.3 A reliable database server with transactions (my_db_rel.erl)
    46> my_db_rel:lock().
    STATE BUSY
    47> my_db_rel:write(simon, berlin).
    STATE BUSY
    48>
    User switch command
     --> i 1
     --> c 1
    <0.2517.0>: <0.2510.0> died because: killed
    STATE FREE
    ** exception exit: killed
    48> my_db_rel:read(simon).
    STATE FREE
    {ok,berlin}

