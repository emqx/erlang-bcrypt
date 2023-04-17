# erlang-bcrypt

[![Run test cases](https://github.com/emqx/erlang-bcrypt/actions/workflows/run_test_cases.yaml/badge.svg)](https://github.com/emqx/erlang-bcrypt/actions/workflows/run_test_cases.yaml)

erlang-bcrypt is a wrapper around the OpenBSD Blowfish password hashing
algorithm, as described in ["A Future-Adaptable Password Scheme"](http://www.openbsd.org/papers/bcrypt-paper.ps) by Niels Provos and David Mazieres.

## Basic build instructions

1. Build it (project uses rebar, but I've included a Makefile):

```
make
```

2. Run it (simple way, starting sasl, crypto and bcrypt):

```
erl -pa ebin -boot start_sasl -s crypto -s bcrypt
```

## Basic usage instructions


1. Hash a password using a salt with the default number of rounds:

```erlang
1> {ok, Salt} = bcrypt:gen_salt().
{ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK."}
2> {ok, Hash} = bcrypt:hashpw("foo", Salt).
{ok,"$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6"}
```

2. Verify the password:

```erlang
3> {ok, Hash} =:= bcrypt:hashpw("foo", Hash).
true
4> {ok, Hash} =:= bcrypt:hashpw("bar", Hash).
false
```

## Configuration

The bcrypt application is configured by changing values in the
application's environment:

- `default_log_rounds`
  Sets the default number of rounds which define the complexity of the
  hash function. Defaults to `12`.

- `mechanism`
  Specifies whether to use the NIF implementation (`'nif'`) or a
  pool of port programs (`'port'`). Defaults to `'nif'`.

  Note: the NIF implementation no longer blocks the Erlang VM
  scheduler threads

  Note: We have delete the `port` mechanism after 0.5.4 for some
  compiling error on OTP 23. if you want to re-introduce it, please see
  `backup_bcrypt_port` branch - EMQX Team

- `pool_size`
  Specifies the size of the port program pool. Defaults to `4`.

## Authors

- [Hunter Morris](http://github.com/skarab)
- [Mrinal Wadhwa](http://github.com/mrinalwadhwa)
