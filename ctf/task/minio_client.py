from minio import Minio
from django.conf import settings
from django.core.cache import cache


def get_minio_client(admin=False):
    if admin:
        return Minio(
            settings.MINIO_URL,
            access_key=settings.MINIO_ACCESS_KEY,
            secret_key=settings.MINIO_SECRET_KEY,
            secure=settings.MINIO_SECURE
        )
    else:
        return Minio(
            settings.MINIO_URL,
            access_key=settings.MINIO_USER_ACCESS_KEY,
            secret_key=settings.MINIO_USER_SECRET_KEY,
            secure=settings.MINIO_SECURE
        )


def get_presigned_url(file_name):
    url = cache.get(file_name)
    if not url:
        minio_client = get_minio_client()
        bucket_name = settings.MINIO_BUCKET_NAME
        url = minio_client.presigned_get_object(bucket_name, file_name)
        cache.set(file_name, url, timeout=86400)
    return url
