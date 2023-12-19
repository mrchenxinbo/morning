del src\*.erl
del src_db_mods\*.erl
del include\model_*.hrl
del schema\schema.sql
del schema\log_schema.sql

pause

erl -noshell -s make_mysql_schema run -pre "zc_mhdl_"
erl -noshell -s make_model_def run
erl -noshell -s make_model_impl run
erl -noshell -s make_db_impl run

pause
