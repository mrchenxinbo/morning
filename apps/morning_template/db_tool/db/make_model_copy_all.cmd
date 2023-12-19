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

copy include\model_def.hrl ..\..\game_server\include\
copy include\model_log_def.hrl ..\..\game_server\include\
copy src\*.erl ..\..\game_server\src\db_models\

pause
