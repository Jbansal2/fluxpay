COBC     = cobc
CFLAGS   = -Wall -I copybooks
# EVERY PROGRAM EXCEPT THE MAIN MENU IS BUILT AS A CALLABLE MODULE
MAINS    = BANKMAIN RUNPGM
MODULES  = $(filter-out $(MAINS),$(basename $(notdir $(wildcard src/*.cbl))))

all: bin/bankmain bin/runpgm $(MODULES:%=bin/%.so)

bin:
	mkdir -p bin data build

bin/bankmain: src/BANKMAIN.cbl copybooks/*.cpy | bin
	$(COBC) -x $(CFLAGS) -o $@ $<

bin/runpgm: src/RUNPGM.cbl copybooks/*.cpy | bin
	$(COBC) -x $(CFLAGS) -o $@ $<

bin/%.so: src/%.cbl copybooks/*.cpy | bin
	$(COBC) -m $(CFLAGS) -o $@ $<

# EMBEDDED-SQL PROGRAMS (OPTIONAL): NEED OCESQL AND POSTGRESQL
OCESQL   = ocesql
ESQLCOPY = /usr/local/share/open-cobol-esql/copy
SQLPGMS  = SQLLOAD SQLRPT SQLINQ

sql: $(SQLPGMS:%=bin/%.so)

bin/%.so: sql/%.cbl copybooks/*.cpy | bin
	mkdir -p build
	$(OCESQL) $< build/$*.cob
	$(COBC) -m -fstatic-call $(CFLAGS) -I $(ESQLCOPY) -o $@ \
	    build/$*.cob -L/usr/local/lib -locesql

sqlschema:
	PGPASSWORD=$${CB_DB_PASS:-corebank} psql -h localhost \
	    -U $${CB_DB_USER:-corebank} corebank -f sql/schema.sql

run: all
	COB_LIBRARY_PATH=bin ./bin/bankmain

test: all unit
	./tests/run_tests.sh

unit: all
	./tests/unit/run_unit.sh

clean:
	rm -rf bin build

.PHONY: all run test unit clean sql sqlschema
