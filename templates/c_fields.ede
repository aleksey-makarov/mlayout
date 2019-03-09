{% for field in fs %}
/* {{ field.key }}: {{field.value.name}} */
{%   if field.value.offset.from %}
#define {{prefix}}_{{field.key}}_from {{field.value.offset.from}}
#define {{prefix}}_{{field.key}}_to {{field.value.offset.to}}
{%     if field.value.values %}
{%       for v in field.value.values %}
#define  {{prefix}}_{{field.key}}_{{v.key}}_value {{v.value.value}}{% if v.value.name %} /* {{v.key}}: {{v.value.name}} */{% endif %}
{%       endfor %}
{%     endif %}
{%   else %}
#define {{prefix}}_{{field.key}} (1 << {{field.value.offset}})
{%     if field.value.values %}
{%       for v in field.value.values %}
#define  {{prefix}}_{{field.key}}_{{v.key}} ({{v.value.value}} << {{field.value.offset}}){% if v.value.name %} /* {{v.key}}: {{v.value.name}} */{% endif %}
{%       endfor %}
{%     endif %}
{%   endif %}

{% endfor %}
