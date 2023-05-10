

# Module bson #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-array">array()</a> ###


<pre><code>
array() = list()
</code></pre>




### <a name="type-bin">bin()</a> ###


<pre><code>
bin() = #bson.binary{type = binary | function | uuid | md5 | $$, value = binary()}
</code></pre>




### <a name="type-datetime">datetime()</a> ###


<pre><code>
datetime() = <a href="#/erts/doc/erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>




### <a name="type-document">document()</a> ###


<pre><code>
document() = map() | list()
</code></pre>




### <a name="type-javascript">javascript()</a> ###


<pre><code>
javascript() = #bson.javascript{value = binary()}
</code></pre>




### <a name="type-long">long()</a> ###


<pre><code>
long() = #bson.long{value = integer()}
</code></pre>




### <a name="type-max_key">max_key()</a> ###


<pre><code>
max_key() = MAX_KEY
</code></pre>




### <a name="type-min_key">min_key()</a> ###


<pre><code>
min_key() = MIN_KEY
</code></pre>




### <a name="type-objectid">objectid()</a> ###


<pre><code>
objectid() = #bson.objectid{value = binary()}
</code></pre>




### <a name="type-regexp">regexp()</a> ###


<pre><code>
regexp() = #bson.regexp{value = [binary()]}
</code></pre>




### <a name="type-timestamp">timestamp()</a> ###


<pre><code>
timestamp() = #bson.timestamp{value = integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary_to_objectid-1">binary_to_objectid/1</a></td><td>Returns the <code>bson:objectid()</code> representation of <code>Id</code>
where <code>Id</code> is a BSON ObjectId in <code>hexadecimal</code> string format.</td></tr><tr><td valign="top"><a href="#construct-1">construct/1</a></td><td>Constructs a BSON binary based on <code>Spec</code>
where <code>Spec</code> is a list of tuples such as <code>{Encoding, Value}</code>
where <code>Encoding</code> is a valid <code>bson_encoder</code> function
and <code>Value</code> is the value to encode using the specified encoding.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Returns <code>{Document, Remainder}</code> based on <code>Payload</code>
where <code>Document</code> is the decoded BSON document (<code>bson:document()</code>)
and <code>Remainder</code> is the bytes remaining from <code>Payload</code>.</td></tr><tr><td valign="top"><a href="#destruct-2">destruct/2</a></td><td>Desctructs a BSON binary (<code>Payload</code>) based on <code>Spec</code>
where <code>Spec</code> is a list of atoms, each representing a valid
<code>bson_decoder</code> function.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Returns the BSON representation of <code>Document</code>.</td></tr><tr><td valign="top"><a href="#objectid_to_binary-1">objectid_to_binary/1</a></td><td>Returns the <code>hexadecimal</code> string representation of <code>Id</code>
where <code>Id</code> is a <code>bson:objectid()</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary_to_objectid-1"></a>

### binary_to_objectid/1 ###

<pre><code>
binary_to_objectid(Id::binary()) -&gt; <a href="#type-objectid">objectid()</a>
</code></pre>
<br />

Returns the `bson:objectid()` representation of `Id`
where `Id` is a BSON ObjectId in `hexadecimal` string format.

<a name="construct-1"></a>

### construct/1 ###

<pre><code>
construct(Spec::list()) -&gt; binary()
</code></pre>
<br />

Constructs a BSON binary based on `Spec`
where `Spec` is a list of tuples such as `{Encoding, Value}`
where `Encoding` is a valid `bson_encoder` function
and `Value` is the value to encode using the specified encoding.

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Payload::binary()) -&gt; {<a href="#/bson/doc/bson.md#type-document">bson:document()</a>, binary()}
</code></pre>
<br />

Returns `{Document, Remainder}` based on `Payload`
where `Document` is the decoded BSON document (`bson:document()`)
and `Remainder` is the bytes remaining from `Payload`.

<a name="destruct-2"></a>

### destruct/2 ###

<pre><code>
destruct(Spec::list(), Payload::binary()) -&gt; list()
</code></pre>
<br />

Desctructs a BSON binary (`Payload`) based on `Spec`
where `Spec` is a list of atoms, each representing a valid
`bson_decoder` function.

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Document::<a href="#/bson/doc/bson.md#type-document">bson:document()</a>) -&gt; binary()
</code></pre>
<br />

Returns the BSON representation of `Document`.

<a name="objectid_to_binary-1"></a>

### objectid_to_binary/1 ###

<pre><code>
objectid_to_binary(X1::<a href="#type-objectid">objectid()</a>) -&gt; binary()
</code></pre>
<br />

Returns the `hexadecimal` string representation of `Id`
where `Id` is a `bson:objectid()`.

