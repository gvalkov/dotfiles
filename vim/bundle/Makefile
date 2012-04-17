SHELL := /bin/bash
.PHONY = export status

export: 
	@while read name url; do             \
		if [[ -e "$$name" ]]; then       \
			echo "$$name ... skipped" ;  \
			continue;                    \
		fi;                              \
		\
		case $$(echo $$name | awk -F. '{print $$NF}') in         \
			git)                                                 \
    			git clone $$url $$name && rm -rf "./$$name/.git" \
				;;                                               \
			hg)                                                  \
    			hg clone $$url $$name && rm -rf "./$$name/.hg"   \
				;; \
		esac \
	done < bundles.txt
			
status:
	@while read name url; do                               \
		[[ -e "$$name" ]] && s='installed' || s='missing'; \
		[[ (-d "$$name/.git") || (-d "$$name/.hg") ]] && c='clone' || c='export'; \
		printf '%-23s %-10s %-10s\n' "$$name" "$$s" "$$c" ; \
	done < bundles.txt

clean:
	@while read name url; do             \
		if [[ -e "$$name" ]]; then       \
			echo "removing $$name";      \
			rm -rf "./$$name";           \
		fi                               \
	done < bundles.txt



#vim:set ft=make ai noet sw=8 sts=8:

