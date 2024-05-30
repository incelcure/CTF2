from prometheus_client import Counter, Histogram

registration_counter = Counter('django_user_registrations_total', 'Total number of user registrations')
login_counter = Counter('django_user_logins_total', 'Total number of user logins')

attempt_counter = Counter('django_task_attempts_total', 'Total number of task attempts')

