import logging
import os
import time
import fnmatch
import ifaddr
from django.utils.deprecation import MiddlewareMixin
from prometheus_client import Counter
from .metrics import request_latency_histogram, response_size_histogram, server_error_counter, client_error_counter
from django.http import HttpRequest, HttpResponseForbidden
from dotenv import load_dotenv

load_dotenv()

unrestricted_hosts_str = os.getenv('UNRESTRICTED_HOSTS', '')
UNRESTRICTED_HOSTS = list(filter(None, unrestricted_hosts_str.split(',')))
RESTRICTED_ENDPOINTS = ['/metrics', '/admin']
UNRESTRICT_INTERFACES = os.getenv('UNRESTRICT_INTERFACES', str(False)).lower() in ('true', '1', 't')

if UNRESTRICT_INTERFACES:
    for adapter in ifaddr.get_adapters():
        for ip in adapter.ips:
            UNRESTRICTED_HOSTS.append(ip.ip[0] if isinstance(ip.ip, tuple) else ip.ip)

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

    def process_request(self, request: HttpRequest):
        host = request.get_host().split(':')[0]
        if any(request.path.startswith(x) for x in RESTRICTED_ENDPOINTS) \
            and not any(fnmatch.fnmatch(host, x) for x in UNRESTRICTED_HOSTS):
            return HttpResponseForbidden("Access forbidden: this endpoint is restricted")
        return None
