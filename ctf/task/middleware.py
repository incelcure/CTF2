import time
from django.utils.deprecation import MiddlewareMixin
from prometheus_client import Counter
from .metrics import request_latency_histogram, response_size_histogram, server_error_counter, client_error_counter


class MetricsMiddleware(MiddlewareMixin):
    def process_request(self, request):
        request.start_time = time.time()

    def process_response(self, request, response):
        response_time = time.time() - request.start_time
        request_latency_histogram.labels(method=request.method, endpoint=request.path).observe(response_time)
        response_size_histogram.labels(method=request.method, endpoint=request.path).observe(len(response.content))

        if response.status_code >= 500:
            server_error_counter.inc()
        elif response.status_code >= 400:
            client_error_counter.inc()

        return response
