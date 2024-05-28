from minio import Minio
from django.conf import settings


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
