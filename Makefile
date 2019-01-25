GIT    = git

parts = qlua hdf5 aff qopqdp qdp qio qla qmp lua gsl arpack lapack clover mdwf qa0 sfc twisted quda

.PHONY: all update clean versions $(parts) $(parts:%=clean-%)

ifeq ("$(TARGET)","")
all clean:
	@echo "Unspecified TARGET" 1>&2
	exit 1
else
include ./configs/$(TARGET)

all: qlua versions

clean:
	-for n in $(parts) ; do \
	  echo "** make clean in $$n"; \
	  rm -f logs/$$n ; \
	  [ -f parts/$$n/Makefile ] && make -C parts/$$n clean ; \
	done
endif

$(parts:%=clean-%):
	rm -f $(@:clean-%=logs/%)
	make -C $(@:clean-%=parts/%) clean

update:
	$(GIT) submodule init
	$(GIT) submodule update
	for p in $(parts) ; do \
	  echo "** make update in $$p"; \
	  (cd parts/$$p/tree && $(GIT) submodule init && $(GIT) submodule update) || exit 1; \
	done


versions:
	[ -d $(PREFIX)/versions ] || mkdir -p $(PREFIX)/versions
	[ -f configs/$(TARGET) ] && cp configs/$(TARGET) $(PREFIX)/versions/target
	$(GIT) submodule status --recursive | sed -e 's/ (.*$$//' > $(PREFIX)/versions/modules 
	$(GIT) submodule status \
        | sed -e 's/parts.//;s?/.*$$??' \
        | awk '{v[$$2]=$$1;} \
           END{for (k in v) { z = toupper(k); printf "#ifdef %s_VERSION\n#undef %s_VERSION\n#endif\n", z, z; \
                  printf "#define %s_VERSION \"%s\"\n", z, v[k]; }}' \
        > parts/qlua/tree/versions.inc.in

$(parts): % : logs/%

$(parts:%=logs/%):
	make -C $(@:logs/%=parts/%) TARGET=$(TARGET)
	[ -d logs ] || mkdir logs
	touch $@

logs/qio:     logs/qmp
logs/qdp:     logs/qmp logs/qio logs/qla
logs/qa0:     logs/sfc
logs/clover:  logs/qa0 logs/qmp logs/gsl logs/qdp
logs/twisted: logs/qa0 logs/qmp logs/gsl logs/qdp
logs/mdwf:    logs/qa0 logs/qmp logs/gsl
logs/qopqdp:  logs/qmp logs/qla logs/qio logs/qdp
logs/quda:    logs/qmp
logs/qlua:    logs/lua logs/lapack logs/arpack logs/gsl logs/aff logs/hdf5 logs/qdp \
                logs/qopqdp logs/clover logs/mdwf logs/twisted logs/quda

logs/qlua:    parts/qlua/tree/versions.inc.in

parts/qlua/tree/versions.inc.in: versions
