{% for value in vs %}
#define  {{prefix}}_{{value.key}}	{{value.value.value}} /* {{value.value.name}} */
{% endfor %}
