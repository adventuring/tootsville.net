# Makefile for Tootsville
#
# Targets:
#
# deps — install prerequisites needed from the build-deps file
#
# bin — compile the deployment files (CGI script, compiled JS/CSS, &c)
#
# doc — extract and build documentation in various formats
#
# doc-publish — push docs to Goethe
#
# test — run automated unit tests
#
# ui-test — run Selenium-based unit tests
#
# deploy — send code to production server, compile and test
#
# 
#
# Copyright © 2008-2017 Bruce-Robert Pocock; © 2018,2019 The Corporation
# for Inter-World Tourism and Adventuring (ciwta.org).
#
# This program is  Free Software: you can redistribute  it and/or modify
# it  under the  terms  of  the GNU  Affero  General  Public License  as
# published by  the Free  Software Foundation; either  version 3  of the
# License, or (at your option) any later version.
#
# This program  is distributed in the  hope that it will  be useful, but
# WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
# MERCHANTABILITY  or FITNESS  FOR  A PARTICULAR  PURPOSE.  See the  GNU
# Affero General Public License for more details.
#
# You  should have  received a  copy of  the GNU  Affero General  Public
# License     along    with     this     program.     If    not,     see
# <https://www.gnu.org/licenses/>.
#
# You can reach CIWTA at https://ciwta.org/, or write to us at:
#
# PO Box 23095
#
# Oakland Park, FL 33307-3095
#
# USA
#
# 

all:	bin doc

clean:
	-rm -rf Tootsville Tootsville.new \
		doc/* bin/buildapp
	-rm -rf $$HOME/.cache/common-lisp/*/$$(pwd)
	find . -name \*.fasl -exec rm {} \;
	find . -name dumper\*.lisp -exec rm {} \;
	find . -name \*~ -exec rm {} \;
	find . -name \*.orig -exec rm {} \;
	-rm -f TODO.org TODO.scorecard	
	git gc --auto || true

modules:	../.gitmodules $(shell  grep '\[submodule "' ../.gitmodules | \
			cut -d '"' -f 2 | sed -e 's,^,../.git/modules/,g' -e 's,$$,/config,g')
	if [ -d ../.git ]; then cd .. ; git submodule update --init; fi

../.git/modules/%/config:	.gitmodules
	(cd .. ; git submodule update --init )

TAGS:	$(shell find . -name \*.lisp)
	ctags --languages=lisp -e -R -f TAGS

deploy:	bin test server-push doc-publish deploy-servers

VERSION=$(shell grep :version tootsville.asd | cut -d '"' -f 2)
doc-publish:	doc
	rsync -rv -essh doc/* goethet@goethe.tootsville.org:goethe.tootsville.org/devel/docs/Tootsville/$(VERSION)/

bin:	Tootsville

~/quicklisp/setup.lisp:	src/setup.lisp
	sbcl --non-interactive \
		--load src/setup.lisp \
		--quit

../.gitmodules:
	>> ../.gitmodules

bin/buildapp:
	if which buildapp; \
	then \
		ln -s $$(which buildapp) -f bin/buildapp; \
	else \
		sbcl --non-interactive \
			--load ~/quicklisp/setup.lisp \
			--eval '(ql:quickload :buildapp)' \
			--eval '(buildapp:build-buildapp "bin/buildapp")'; \
	fi

Tootsville:	Tootsville.new
	./Tootsville.new check
	mv --backup=simple Tootsville.new Tootsville

Tootsville.new:	quicklisp-manifest.tmp bin/buildapp
	bin/buildapp --output ../Tootsville.new \
		--manifest-file quicklisp-manifest.tmp \
		--load src/setup.lisp \
		--eval '(ql:quickload :Tootsville)' \
		--entry Tootsville:entry

quicklisp-manifest.tmp:	tootsville.asd \
		$(shell find . -name \*.lisp \
			-and -not -path \**/.\* \
			-or -name \*.asd \
			-and -not -path \**/.\*)
	sbcl --no-userinit --no-sysinit --non-interactive \
		--load src/setup.lisp \
		--eval '(ql:quickload :Tootsville)' \
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.tmp")'
	for asd in src/lib/*/*.asd ; \
	do \
		echo $$(pwd)/$$asd >> quicklisp-manifest.tmp ;\
	done
	echo $$(pwd) >> quicklisp-manifest.tmp

test:	bin
	./Tootsville check

doc:	doc/Tootsville.texi

doc/Tootsville.texi:	Tootsville
	./Tootsville write-docs

install:	tootsville.service Tootsville
	cp Tootsville --backup=simple -f /usr/local/bin/
	cp tootsville.service --backup=simple -f /usr/lib/systemd/system/ || \
		cat tootsville.service > /usr/lib/systemd/system/tootsville.service
	mkdir -p ~/.config/Tootsville/
	mv --backup=simple ~/Tootsville.config.lisp ~/.config/Tootsville/
	cp 55-tootsville.conf -f /etc/rsyslog.conf
	sudo -n systemctl enable tootsville || :
	sudo -n systemctl restart tootsville || :
	sudo -n systemctl start tootsville

test: servers-test

####################

.deps~:	build/build-deps bin/do-install-deps
	bin/do-install-deps
	>> ~/.sbclrc
	>.deps~

####################

servers-test:	./Tootsville
	./Tootsville check

#################### vars

# To target alternate clusters:
#	make CLUSTER=qa
#	make CLUSTER=.
CLUSTER:=test
ifeq ($(CLUSTER),.)
clusterorg=tootsville.org
clusternet=tootsville.net
else
clusterorg=$(CLUSTER).tootsville.org
clusternet=$(CLUSTER).tootsville.net
endif

GAMEHOSTS=game3
BALANCERS=balancer1 balancer2

LOCAL_USERNAME=$(shell whoami)
REVISION=$(shell git log -n 1 --pretty=format:"%H")
REALNAME:=$(shell if which finger &>/dev/null ;\
	then \
	    REALNAME=$$(finger $$LOCAL_USERNAME | perl -ne 'if (/Name: (.*)[\t\n]/){print $$1; exit}')  ;\
	fi ;\
	if [ "x$$REALNAME" = "x" ] ;\
	then \
	    REALNAME=$$(grep ^$$LOCAL_USERNAME: /etc/passwd | cut -d: -f5 | cut -d, -f1) ;\
	fi ;\
	if [ "x$$REALNAME" = "x" ] ;\
	then \
	    REALNAME=$$(whoami) ;\
	fi ;\
	echo $$REALNAME)

# Rollbar
ACCESS_TOKEN=7c28543f4257495694b50fe59acb2ada

#################### servers

servers:	./Tootsville

./Tootsville:	$(shell find servers \( -name \*.lisp -o -name \*.asd \) -and -not -name .\*)
	$(MAKE) -C servers Tootsville test

#################### doc

doc:	server-doc js-doc

server-doc: \
	doc/Tootsville.txt \
	doc/Tootsville.pdf \
	doc/Tootsville.html.tar.gz \
	doc/Tootsville.info

doc-install-info:	doc/Tootsville.info
	install-info doc/Tootsville.info /usr/local/share/info/dir

doc/Tootsville.texi:	./doc/Tootsville.texi
	cp ./doc/Tootsville.texi doc/

./doc/Tootsville.texi: ./Tootsville
	$(MAKE) -C servers doc/Tootsville.texi

doc/Tootsville.html.tar.gz:	doc/Tootsville.html.tar
	gzip -9 -c < $< > $@

doc/Tootsville.html.tar.Z:	doc/Tootsville.html.tar
	compress -9 -c < $< > $@

doc/Tootsville.html.tar.bz2:	doc/Tootsville.html.tar
	bzip2 -9 -c < $< > $@

doc/Tootsville.html.tar.xz:	doc/Tootsville.html.tar
	xz -9 -c < $< > $@

doc/Tootsville.html.tar:	doc/Tootsville.html.d/index.html
	cd doc; tar cf Tootsville.html.tar Tootsville.html.d

doc/Tootsville.html.zip:	doc/Tootsville.html.d/index.html
	cd doc; zip -9 Tootsville.html.zip Tootsville.html.d

doc/Tootsville.html.d/index.html:	doc/Tootsville.texi doc/doc.css
	cd doc; makeinfo -o Tootsville.html.d/ \
		--html --css-include=doc.css \
		--split=node Tootsville.texi

doc/Tootsville.ps:	doc/Tootsville.pdf
	cd doc; pdf2ps Tootsville.pdf

doc/Tootsville.pdf:	doc/Tootsville.texi
	cd doc; PDFLATEX=xelatex texi2pdf Tootsville.texi 

doc/Tootsville.txt:	doc/Tootsville.texi
	cd doc; makeinfo --plaintext -o Tootsville.txt Tootsville.texi

doc/Tootsville.info:	doc/Tootsville.texi
	cd doc; makeinfo -o Tootsville.info Tootsville.texi

doc/doc.css:	www/doc.less

all-docs: \
	doc/Tootsville.html.tar.gz	\
	doc/Tootsville.html.tar.Z	\
	doc/Tootsville.html.tar.bz2	\
	doc/Tootsville.html.tar.xz	\
	doc/Tootsville.html.zip	\
	doc/Tootsville.ps	\
	doc/Tootsville.pdf	\
	doc/Tootsville.txt 	\
	doc/Tootsville.info

#################### htaccess

htaccess:	dist/htaccess.all/play.$(clusterorg).htaccess

dist/htaccess.all/play.$(clusterorg).htaccess:	build/htaccess.base bin/make-all-htaccess
	bin/make-all-htaccess

#################### /worker.js

worker:	dist/worker.js

dist/worker.js:	worker/Worker.js worker/WorkerStart.js worker/TootsvilleWorker.js
	mkdir -p dist/
	closure-compiler --create_source_map dist/worker.map \
                    $(< build/closure-compiler.opts)           \
		--js worker/TootsvilleWorker.js            \
		--js worker/Worker.js                      \
		--js worker/WorkerStart.js                 \
		--js_output_file $@
	echo '//# sourceMappingURL=/worker.map' >> $@

#################### dist/node-adopt.js

node_modules/.bin/browserify:	package-lock.json
	npm install browserify --save

node_modules/@openid/openyolo/package.json:
	npm install @openid/openyolo --save

dist/node-adopt.js:	build/node-adopt.js \
		node_modules/@openid/openyolo/package.json \
		package-lock.json node_modules/.bin/browserify
	node_modules/.bin/browserify build/node-adopt.js -o dist/node-adopt.js

#################### dist/play/play.js

dist/play/play.js:	build/js.order $(shell cat build/js.order)
	mkdir -p dist/play/
	closure-compiler --create_source_map dist/play/play.map   \
		--third_party                                   \
                    $(< build/closure-compiler.opts)                \
		--source_map_location_mapping 'play/|/play/'        \
		--language_in ECMASCRIPT6                        \
		--language_out ECMASCRIPT5_STRICT                \
		$$(< build/js.order )                            \
		--js_output_file $@
	echo '//# sourceMappingURL=/play/play.map' >> $@

dist/play/stun-list.js:	build/public-stun-list.txt
	echo '/* File generated from build/public-stun-list.txt by Makefile */'  > dist/play/stun-list.js
	echo 'if (!("Tootsville" in window)) { Tootsville = { gossip: {} }; };'  >> dist/play/stun-list.js
	echo 'if (!("gossip" in Tootsville)) { Tootsville.gossip = {}; };'  >> dist/play/stun-list.js
	echo 'Tootsville.gossip.stunList = ['  >> dist/play/stun-list.js
	(while read stun ; do echo '"'$$stun'",' >> dist/play/stun-list.js; done) < build/public-stun-list.txt
	echo '];'  >> dist/play/stun-list.js

play:	dist/play.$(clusterorg)

dist/play/play.map:	dist/play/play.js

#################### dist/play/play.css

PLAYLESSDEPS=$(wildcard play/*.less play/**/*.less)

dist/play/play.css:	$(PLAYLESSDEPS)
	mkdir -p dist/play/
	lessc --strict-math=yes --source-map play/play.less dist/play/play.css

dist/play/play.css.map:	dist/play/play.css

#################### TODO

TODO.org:	$(shell find */ -name \\*.lisp -o -name \\*.css -o -name \\*.js -o -name \\*.org -o \
		   -name \\*.texi -o -name \\*.asd -o -name \\*.txt -o -name \\*.html) \
		 README.org
	-mv TODO.org TODO.org~ 2>/dev/null
	echo '* TODO-type notes found $$(date +%Y-%m-%d)' > TODO.org
	echo '' >> TODO.org
	echo '** FIXME Actual bugs!' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn FIXME servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** TODO To be done ASAP' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn TODO servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** XXX Might Be Nice to do someday' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn XXX servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org
	echo '** ☠☠☠ Bruce-Robert should examine this' >> TODO.org
	echo '' >> TODO.org
	git grep -Hn ☠☠☠: servers mesh play www build README.org \
	 | perl -e '$$lastfile = ""; while (<>) { m/^(.*):([0-9]*):(.*)/; if ($$1 ne $$lastfile) { print "*** $$1\n\n"; $$lastfile = $$1 } print "$$2:$$3\n\n" }' >> TODO.org

TODO.scorecard:	$(shell find \( -name \*.lisp -o -name \*.asd \) -and -not -name .\*) \
	README.org
	echo -n 'TOOTS_FIXME=' > TODO.scorecard
	git grep FIXME *.lisp *.asd src/
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_TODO=' >> TODO.scorecard
	git grep TODO *.lisp *.asd src/
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_XXX=' >> TODO.scorecard
	git grep XXX *.lisp *.asd src/
	 | wc -l >> TODO.scorecard
	echo -n 'TOOTS_BRP=' >> TODO.scorecard
	git grep ☠☠☠ *.lisp *.asd src/
	 | wc -l >> TODO.scorecard

#################### dev-test

devel-test:	devel-serve

devel-serve:	./Tootsville
	./Tootsville server < /dev/null

#################### deploy

deploy-servers:	predeploy-servers
	for host in $(GAMEHOSTS) ; \
	do \
		echo " » Deploy $$host.$(clusternet)" ;\
                    scp ~/.config/Tootsville/Tootsville.config.lisp $$host.$(clusternet):.config/Tootsville ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers install ;\
		VERSION=$(shell ./Tootsville version-info version) ;\
		curl https://api.rollbar.com/api/1/deploy/ \
		     -F access_token=$(ACCESS_TOKEN) \
		     -F environment=$$host.$(clusternet) \
		     -F framework=gmake \
		     -F notifier.name=gmake \
		     -F revision=$(REVISION) \
		     -F comment="v $(VERSION)" \
		     -F uuid=$(uuidgen) \
		     -F local_username=$(LOCAL_USERNAME) ;\
	done

predeploy:	no-fixmes predeploy-servers

connectivity:
	echo " » Test connectivity"
	for host in $(GAMEHOSTS); do \
	   ssh $$host.$(clusternet) sbcl --no-userinit --quit | grep 'This is SBCL'; \
	done

no-fixmes:	TODO.scorecard
	TOOT_TODO=$$(grep TODO TODO.scorecard | wc -l) ;\
	TOOT_FIXME=$$(grep FIXME TODO.scorecard | wc -l) ;\
	if [[ $$TOOT_FIXME -gt 0 ]] ;\
	then \
			clear ;\
			echo "There are $$TOOT_FIXME FIXME comments!" ;\
			if [[ "$(CLUSTER)" = . ]] ;\
			then \
				echo " ✗ Refusing to deploy to Production with FIXME notes" ;\
				exit 8 ;\
			fi ;\
			echo "" ;\
			echo "$(REALNAME), are you sure you want to deploy to $(clusterorg)" ;\
			echo "when there are $$TOOT_FIXME FIXME notes and $$TOOT_TODO TODOs?" ;\
			echo "" ;\
			read -p "Deploy anyway? (Y/N) ⇒ " -n 1 yorn ;\
			if [[ "$${yorn}" = "y" ]] ;\
			then \
				echo "" ;\
				echo "Overridden" ;\
				echo "" ;\
				echo "Override accepted. Good luck …" ;\
				break ;\
			elif [[ "$${yorn}" = "n" ]] ;\
			then \
				echo "" ;\
				echo "That seems wise. Exiting." ;\
				echo "" ;\
				exit 8 ;\
			else \
				echo "Only y or n work. Treading $$yorn as No." ; \
				exit 8; \
			fi ;\
	fi

predeploy-servers:	servers quicklisp-update-servers
	for host in $(GAMEHOSTS) ;\
	do \
		echo " » Pre-deploy $$host.$(clusternet)" ;\
		rsync -essh --delete -zar * .??* $$host.$(clusternet):tootsville.org/ ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers clean || exit 6 ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers Tootsville || exit 6 ;\
		ssh $$host.$(clusternet) make -C tootsville.org/servers test || exit 6 ;\
	done

quicklisp-update-servers:
	for host in $(GAMEHOSTS) ; \
	do \
		echo " » Ensure latest Quicklisp on $$host.$(clusternet)" ;\
	    ssh $$host.$(clusternet) \
	        sbcl --non-interactive \
	        --no-inform \
	        --eval "'(ql:update-client)'" \
	        --eval "'(ql:update-all-dists)'" \
	        --quit ;\
	done

remotes:
	if ! git remote -v | grep github &>/dev/null ;\
	then \
		git remote add github git@github.com:adventuring/tootsville.org ;\
	fi
	if ! git remote -v | grep gitlab &>/dev/null ;\
	then \
		git remote add gitlab git@gitlab.com:adventuring/tootsville.org ;\
	fi
	if ! git remote -v | grep goethe &>/dev/null ;\
	then \
		git remote add goethe goethe.Tootsville.org:devel/git/tootsville.org ;\
	fi

bump-next-version:
	git status | grep modified: | grep ./tootsville.asd && exit 9 || :
	perl -pne 's/:version "(\d+\.\d+)\.(\d+)"/ ":version \"$$1." . (1+ $$2) . "\"" /e' -i ./tootsville.asd
	git add ./tootsville.asd
	git commit -m "bump version number for next build"

git-tag-deployment:
	VERSION=$$(./Tootsville version-info version) ;\
	now=$$(date +%Y-%m-%d) ;\
	msg="Deployed v$$VERSION to $(clusterorg) $$now" ;\
	if git rev-parse v$$VERSION &>/dev/null ;\
	then \
	    echo "Previous tag v$$VERSION found, adding v$$VERSION-$$now" ;\
	    if git rev-parse v$$VERSION-$$now &>/dev/null ;\
	    then \
	        now=$$(date +%Y-%m-%d.%H%M) ;\
	        msg="Deployed v$$VERSION to $(clusterorg) $$now" ;\
	        echo " - I meant v$$VERSION-$$now" ;\
	        git submodule foreach git tag -a tootsville-v$$VERSION-$$now -m "for Tootsville.org: $$msg" ;\
	        git tag -a v$$VERSION-$$now -m "$$msg" ;\
	    else \
	        git submodule foreach git tag -a tootsville-v$$VERSION-$$now -m "for Tootsville.org: $$msg" ;\
	        git tag -a v$$VERSION-$$now -m "$$msg" ;\
	    fi ;\
	else \
	    echo "First deploy of v$$VERSION, tagging" ;\
	    git submodule foreach git tag -a tootsville-v$$VERSION -m "for Tootsville.org: $$msg" ;\
	    git tag -a v$$VERSION -m "$$msg" ;\
	fi

	git push --tags origin ||:
	git submodule foreach --recursive 'git push --tags origin ||:'
	git push --tags github ||:
	git submodule foreach --recursive 'git push --tags github ||:'
	git push --tags gitlab ||:
	git submodule foreach --recursive 'git push --tags gitlab ||:'
	git push --tags goethe ||:
	git submodule foreach --recursive 'git push --tags goethe ||:'

	$(MAKE) bump-next-version

#################### deploy-docs

deploy-docs:
	make -C servers doc-publish
	scp dist/htaccess.all/goethe.tootsville.net.htaccess goethe.tootsville.org:goethe.tootsville.org/.htaccess
	scp www/favicon.??? goethe.tootsville.org:goethe.tootsville.org/
	rsync -essh -zar www/error goethe.tootsville.org:goethe.tootsville.org/

####################

TAGS:	$(shell find . -type f -name *.lisp)
	etags --declarations $(shell find . -type f -name *.lisp) Makefile