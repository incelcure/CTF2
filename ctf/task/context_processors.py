from django.conf import settings
from .minio_client import get_presigned_url


def logo_url(request):
    return {
        'logo_url': get_presigned_url("logo.png")
    }
