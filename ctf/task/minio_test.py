from minio import Minio
from django.conf import settings
import io
from minio.error import S3Error


def upload_file_to_minio():
    minio_client = Minio(
        settings.MINIO_URL,
        access_key=settings.MINIO_ACCESS_KEY,
        secret_key=settings.MINIO_SECRET_KEY,
        secure=settings.MINIO_SECURE
    )

    bucket_name = settings.MINIO_BUCKET_NAME
    file_name = "task_test.json"
    with open(f'/home/harpoon/Source/django/task_test.json', 'rb') as f:
        file_content = f.read()
    # file_content = "This is a test bruh."
    print('goods')
    # Ensure the bucket exists
    if not minio_client.bucket_exists(bucket_name):
        minio_client.make_bucket(bucket_name)

    # Upload the file
    minio_client.put_object(bucket_name, file_name, data=io.BytesIO(file_content), length=len(file_content))

    print(f"Uploaded {file_name} to {bucket_name}")


def get_file_from_minio():
    minio_client = Minio(
        settings.MINIO_URL,
        access_key=settings.MINIO_ACCESS_KEY,
        secret_key=settings.MINIO_SECRET_KEY,
        secure=settings.MINIO_SECURE
    )

    bucket_name = settings.MINIO_BUCKET_NAME
    file_name = "bruh.txt"

    # Get the file
    response = minio_client.get_object(bucket_name, file_name)
    file_data = response.read().decode()

    print(f"Downloaded {file_name} from {bucket_name}: {file_data}")


def list_objects_from_minio():
    minio_client = Minio(
        settings.MINIO_URL,
        access_key=settings.MINIO_USER_ACCESS_KEY,
        secret_key=settings.MINIO_USER_SECRET_KEY,
        secure=settings.MINIO_SECURE
    )

    try:
        objects = minio_client.list_objects(settings.MINIO_BUCKET_NAME)
        for obj in objects:
            print(obj.object_name)
            print(obj)
    except S3Error as e:
        print(f'Error: {e}')


upload_file_to_minio()
get_file_from_minio()
list_objects_from_minio()
