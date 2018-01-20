ameo
====

a Redis compatible, distributed in-memory key-value store and pubsub server
implemented using riak_core that exposes the topics via WebSockets.

Read more about it here: http://marianoguerra.org/posts/ameo-redis-compatible-getsetdel-and-publishsubscribe-on-riak_core-with-websocket-api/

Demo:

.. image:: https://img.youtube.com/vi/m5NbpmvyQQY/0.jpg)
   :target: https://www.youtube.com/watch?v=m5NbpmvyQQY

Build
-----

::

    make release

Run
---

::

    make console

Try
---

Open http://localhost:8080/ if running one node.

If using clustering with devrel open one or more of the following:

* http://localhost:8198/
* http://localhost:8298/
* http://localhost:8398/

Install redis-tools::

    apt install redis-tools

Use the redis client to interact with ameo::

    redis-cli get foo
    > (nil)

    redis-cli set foo 42
    > OK

    redis-cli get foo
    > 42

    redis-cli del foo
    > (integer) 1

    redis-cli del foo
    > (integer) 0

    redis-cli get foo
    > (nil)

Publish subscribe::

    redis-cli subscribe topic1
    > Reading messages... (press Ctrl-C to quit)

    redis-cli publish topic1 asd
    > (integer) 1

On the subscribe shell::

    > asd

In the cluster::

    redis-cli -p 6479 subscribe topic1

    redis-cli -p 6579 subscribe topic1

    redis-cli -p 6379 publish topic1 "hello topic1 from 6379"
    redis-cli -p 6479 publish topic1 "hello topic1 from 6479"
    redis-cli -p 6579 publish topic1 "hello topic1 from 6579"

    redis-cli -p 6379 publish topic2 "hello topic2 from 6379"
    redis-cli -p 6479 publish topic2 "hello topic2 from 6479"
    redis-cli -p 6579 publish topic2 "hello topic2 from 6579"

Quit
----

::

    2> q().

Play with Clustering
--------------------

Build 3 releases that can run on the same machine::

    make devrel

Start them in different consoles::

    make dev1-console
    make dev2-console
    make dev3-console

join 2 nodes to the first one::

    make devrel-join

check the status of the cluster::

    make devrel-status

you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    joining     0.0%      --      'ameo2@127.0.0.1'
    joining     0.0%      --      'ameo3@127.0.0.1'
    valid     100.0%      --      'ameo1@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:1 / Leaving:0 / Exiting:0 / Joining:2 / Down:0

it should say that 3 nodes are joining, now check the cluster plan::

    make devrel-cluster-plan

it should display the cluster plan, now we can commit the plan::

    make devrel-cluster-commit

check the status of the cluster again::

    make devrel-status

you could see the vnodes transfering::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      75.0%     25.0%    'ameo1@127.0.0.1'
    valid       9.4%     25.0%    'ameo2@127.0.0.1'
    valid       7.8%     25.0%    'ameo3@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

at some point you should see something like this::

    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      33.3%      --      'ameo1@127.0.0.1'
    valid      33.3%      --      'ameo2@127.0.0.1'
    valid      33.3%      --      'ameo3@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:3 / Leaving:0 / Exiting:0 / Joining:0 / Down:0

when you are bored you can stop them::

    make devrel-stop

License
-------

Apache Public License 2.0, see LICENSE file
