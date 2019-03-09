#ifndef __{{filename}}_h__
#define __{{filename}}_h__

/*
 * Generated: {{time}}
 */

{% for word in bitfields %}
/*
 * {{ word.key }}: {{word.value.name}} ({{word.value.width}} bits)
 */

{%   let prefix = word.key %}
{%     if word.value.values %}
{%         include "c_values.ede" with vs = word.value.values %}
{%     endif %}
{%     if word.value.fields %}
{%         include "c_fields.ede" with fs = word.value.fields %}
{%     endif %}
{%   endlet %}
{% endfor %}
{% for mword in mlayout %}
/*
 * {{ mword.key }}: {{mword.value.name}}
 */

{%   for mfield in mword.value.fields %}
/* {{ mfield.key }}: {{mfield.value.name}} */
#define {{mword.key}}_{{mfield.key}}	some_offset
#define {{mword.key}}_{{mfield.key}}_width	{{mfield.value.width}}
{%     let prefix = mword.key | concat ("_") | concat (mfield.key) %}
{%       if mfield.value.values %}
{%         include "c_values.ede" with vs = mfield.value.values %}
{%       endif %}
{%       if mfield.value.fields %}
{%         include "c_fields.ede" with fs = mfield.value.fields %}
{%       endif %}
{%     endlet %}

{%   endfor %}
{% endfor %}
#endif
