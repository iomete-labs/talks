
PRESENT_CMD := \
	present \
	-base /home/user/go/pkg/mod/golang.org/x/tools@v0.1.11/cmd/present

.PHONY: present

present:
	@${PRESENT_CMD}
