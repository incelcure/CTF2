import os
import django
import json
from minio import Minio
from ctf import settings

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'ctf.settings')
django.setup()

from task.models import Task


def load_tasks():
    minio_client = Minio(
        settings.MINIO_URL,
        access_key=settings.MINIO_ACCESS_KEY,
        secret_key=settings.MINIO_SECRET_KEY,
        secure=settings.MINIO_SECURE
    )

    bucket_name = settings.MINIO_BUCKET_NAME
    file_name = 'task3.json'

    try:
        response = minio_client.get_object(bucket_name, file_name)
        file_data = response.read().decode('utf-8')
        tasks_data = json.loads(file_data)

        for task_id, task_data in tasks_data.items():
            defaults = {
                'description': task_data['description'],
                'answer': task_data['answer'],
                'points': task_data['points'],
            }
            if 'file_name' in task_data:
                defaults['file_name'] = task_data['file_name']

            Task.objects.update_or_create(
                title=task_data['title'],
                defaults=defaults
            )
        print('Successfully loaded tasks from JSON file')
    except Exception as e:
        print(f'Error loading tasks: {e}')


load_tasks()
