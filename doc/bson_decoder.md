

# Module bson_decoder #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#array-1">array/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary-1">binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#boolean-1">boolean/1</a></td><td></td></tr><tr><td valign="top"><a href="#byte-1">byte/1</a></td><td></td></tr><tr><td valign="top"><a href="#cstring-1">cstring/1</a></td><td></td></tr><tr><td valign="top"><a href="#datetime-1">datetime/1</a></td><td></td></tr><tr><td valign="top"><a href="#document-1">document/1</a></td><td></td></tr><tr><td valign="top"><a href="#double-1">double/1</a></td><td></td></tr><tr><td valign="top"><a href="#int32-1">int32/1</a></td><td></td></tr><tr><td valign="top"><a href="#int64-1">int64/1</a></td><td></td></tr><tr><td valign="top"><a href="#javascript-1">javascript/1</a></td><td></td></tr><tr><td valign="top"><a href="#objectid-1">objectid/1</a></td><td></td></tr><tr><td valign="top"><a href="#regexp-1">regexp/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-1">string/1</a></td><td></td></tr><tr><td valign="top"><a href="#struct-2">struct/2</a></td><td></td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint32-1">uint32/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint64-1">uint64/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="array-1"></a>

### array/1 ###

<pre><code>
array(Payload::binary()) -&gt; {list(), binary()}
</code></pre>
<br />

<a name="binary-1"></a>

### binary/1 ###

<pre><code>
binary(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-binary">bson:binary()</a>, binary()}
</code></pre>
<br />

<a name="boolean-1"></a>

### boolean/1 ###

<pre><code>
boolean(Payload::binary()) -&gt; {boolean(), binary()}
</code></pre>
<br />

<a name="byte-1"></a>

### byte/1 ###

<pre><code>
byte(Payload::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="cstring-1"></a>

### cstring/1 ###

<pre><code>
cstring(Payload::binary()) -&gt; {binary(), binary()}
</code></pre>
<br />

<a name="datetime-1"></a>

### datetime/1 ###

<pre><code>
datetime(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-datetime">bson:datetime()</a>, binary()}
</code></pre>
<br />

<a name="document-1"></a>

### document/1 ###

<pre><code>
document(Payload::binary()) -&gt; {map(), binary()}
</code></pre>
<br />

<a name="double-1"></a>

### double/1 ###

<pre><code>
double(Payload::binary()) -&gt; {float(), binary()}
</code></pre>
<br />

<a name="int32-1"></a>

### int32/1 ###

<pre><code>
int32(Payload::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="int64-1"></a>

### int64/1 ###

<pre><code>
int64(Payload::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="javascript-1"></a>

### javascript/1 ###

<pre><code>
javascript(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-javascript">bson:javascript()</a>, binary()}
</code></pre>
<br />

<a name="objectid-1"></a>

### objectid/1 ###

<pre><code>
objectid(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-objectid">bson:objectid()</a>, binary()}
</code></pre>
<br />

<a name="regexp-1"></a>

### regexp/1 ###

<pre><code>
regexp(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-regexp">bson:regexp()</a>, binary()}
</code></pre>
<br />

<a name="string-1"></a>

### string/1 ###

<pre><code>
string(Payload::binary()) -&gt; {binary(), binary()}
</code></pre>
<br />

<a name="struct-2"></a>

### struct/2 ###

<pre><code>
struct(Spec::[Function::atom()], Payload::binary()) -&gt; {list(), binary()}
</code></pre>
<br />

<a name="timestamp-1"></a>

### timestamp/1 ###

<pre><code>
timestamp(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-timestamp">bson:timestamp()</a>, binary()}
</code></pre>
<br />

<a name="uint32-1"></a>

### uint32/1 ###

<pre><code>
uint32(Payload::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="uint64-1"></a>

### uint64/1 ###

<pre><code>
uint64(Payload::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

