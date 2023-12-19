

rm -f src/*
rm -f src_db_mods/*
rm -f include/*
rm -f schema/*


erl -pa ebin/ -s make_mysql_schema run -pre ""
erl -pa ebin/ -s make_model_def run
erl -pa ebin/ -s make_model_impl run
erl -pa ebin/ -s make_db_impl run

