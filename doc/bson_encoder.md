

# Module bson_encoder #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#array-1">array/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary-1">binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#boolean-1">boolean/1</a></td><td></td></tr><tr><td valign="top"><a href="#byte-1">byte/1</a></td><td></td></tr><tr><td valign="top"><a href="#cstring-1">cstring/1</a></td><td></td></tr><tr><td valign="top"><a href="#datetime-1">datetime/1</a></td><td></td></tr><tr><td valign="top"><a href="#document-1">document/1</a></td><td></td></tr><tr><td valign="top"><a href="#double-1">double/1</a></td><td></td></tr><tr><td valign="top"><a href="#int32-1">int32/1</a></td><td></td></tr><tr><td valign="top"><a href="#int64-1">int64/1</a></td><td></td></tr><tr><td valign="top"><a href="#javascript-1">javascript/1</a></td><td></td></tr><tr><td valign="top"><a href="#long-1">long/1</a></td><td></td></tr><tr><td valign="top"><a href="#objectid-1">objectid/1</a></td><td></td></tr><tr><td valign="top"><a href="#regexp-1">regexp/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-1">string/1</a></td><td></td></tr><tr><td valign="top"><a href="#struct-1">struct/1</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint32-1">uint32/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint64-1">uint64/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="array-1"></a>

### array/1 ###

<pre><code>
array(Document::list()) -&gt; binary()
</code></pre>
<br />

<a name="binary-1"></a>

### binary/1 ###

<pre><code>
binary(X1::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-bin">bson:bin()</a>) -&gt; binary()
</code></pre>
<br />

<a name="boolean-1"></a>

### boolean/1 ###

<pre><code>
boolean(Value::boolean()) -&gt; binary()
</code></pre>
<br />

<a name="byte-1"></a>

### byte/1 ###

<pre><code>
byte(Value::integer()) -&gt; binary()
</code></pre>
<br />

<a name="cstring-1"></a>

### cstring/1 ###

<pre><code>
cstring(Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="datetime-1"></a>

### datetime/1 ###

<pre><code>
datetime(X1::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-datetime">bson:datetime()</a>) -&gt; binary()
</code></pre>
<br />

<a name="document-1"></a>

### document/1 ###

<pre><code>
document(Document::map() | list()) -&gt; binary()
</code></pre>
<br />

<a name="double-1"></a>

### double/1 ###

<pre><code>
double(Value::float()) -&gt; binary()
</code></pre>
<br />

<a name="int32-1"></a>

### int32/1 ###

<pre><code>
int32(Value::integer()) -&gt; binary()
</code></pre>
<br />

<a name="int64-1"></a>

### int64/1 ###

<pre><code>
int64(Value::integer()) -&gt; binary()
</code></pre>
<br />

<a name="javascript-1"></a>

### javascript/1 ###

<pre><code>
javascript(X1::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-javascript">bson:javascript()</a>) -&gt; binary()
</code></pre>
<br />

<a name="long-1"></a>

### long/1 ###

<pre><code>
long(Value::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-long">bson:long()</a>) -&gt; binary()
</code></pre>
<br />

<a name="objectid-1"></a>

### objectid/1 ###

<pre><code>
objectid(X1::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-objectid">bson:objectid()</a>) -&gt; binary()
</code></pre>
<br />

<a name="regexp-1"></a>

### regexp/1 ###

<pre><code>
regexp(X1::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-regexp">bson:regexp()</a>) -&gt; binary()
</code></pre>
<br />

<a name="string-1"></a>

### string/1 ###

<pre><code>
string(Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="struct-1"></a>

### struct/1 ###

<pre><code>
struct(Spec::[{Function::atom(), Value::term()}]) -&gt; binary()
</code></pre>
<br />

<a name="timestamp-1"></a>

### timestamp/1 ###

<pre><code>
timestamp(X1::<a href="/Users/bajankristof/Projects/Erlang/bson/doc/bson.md#type-timestamp">bson:timestamp()</a>) -&gt; binary()
</code></pre>
<br />

<a name="uint32-1"></a>

### uint32/1 ###

`uint32(Value) -> any()`

<a name="uint64-1"></a>

### uint64/1 ###

<pre><code>
uint64(Value::integer()) -&gt; binary()
</code></pre>
<br />

