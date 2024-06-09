from prometheus_client import Counter, Histogram

registration_counter = Counter('django_user_registrations_total', 'Total number of user registrations')
login_counter = Counter('django_user_logins_total', 'Total number of user logins')

attempt_counter = Counter('django_task_attempts_total', 'Total number of task attempts', ['task_title'])
success_counter = Counter('django_task_attempts_success', 'Total number of success attempts', ['task_title'])

request_latency_histogram = Histogram('django_request_latency_seconds', 'Histogram of request processing time',
                                      ['method', 'endpoint'])
response_size_histogram = Histogram('django_response_size_bytes', 'Histogram of response sizes', ['method', 'endpoint'])

server_error_counter = Counter('django_server_errors_total', 'Total number of server errors (500)')
client_error_counter = Counter('django_client_errors_total', 'Total number of client errors (400)')

jwt_cache_hit = Counter('django_jwt_cache_hit', 'Total number of jwt cache hits')
jwt_cache_miss = Counter('django_jwt_cache_miss', 'Total number of jwt cache misses')
