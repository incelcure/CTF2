import os
import time
from django.utils.deprecation import MiddlewareMixin
from prometheus_client import Counter
from .metrics import request_latency_histogram, response_size_histogram, server_error_counter, client_error_counter
from django.http import HttpResponseForbidden
from dotenv import load_dotenv

load_dotenv()

unrestricted_hosts_str = os.getenv('UNRESTRICTED_HOSTS', '')
UNRESTRICTED_HOSTS = unrestricted_hosts_str.split(',')


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


class HostFilterMiddleware:
    def __init__(self, get_response):
        self.get_response = get_response

    def __call__(self, request):
        response = self.process_request(request)
        if response is None:
            response = self.get_response(request)
        return response

    def process_request(self, request):
        host = request.get_host()
        if request.path.startswith('/metrics') or request.path.startswith('/admin'):
            if host not in UNRESTRICTED_HOSTS:
                return HttpResponseForbidden("Access forbidden: Host not allowed")
        return None
