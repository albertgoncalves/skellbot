# skellbot

Needed things
---
  * [Nix](https://nixos.org/nix/)
  * `path/to/skellbot/.env` exporting Slack user and bot tokens:

```
export SLACK_USER_TOKEN="xoxp-..."
export SLACK_BOT_TOKEN="xoxb-..."
```

Quick start
---
```
$ nix-shell
[nix-shell:path/to/skellbot]$ ./test
[nix-shell:path/to/skellbot]$ ./main
```
