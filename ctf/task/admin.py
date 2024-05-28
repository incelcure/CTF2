from django.contrib import admin
from django import forms
from .models import Task
from .minio_client import get_minio_client
from django.conf import settings


class TaskForm(forms.ModelForm):
    file = forms.FileField(required=False)

    class Meta:
        model = Task
        fields = ['title', 'description', 'answer']

    def save(self, commit=True):
        instance = super().save(commit=False)
        file = self.cleaned_data.get('file')

        if file:
            minio_client = get_minio_client(admin=True)

            bucket_name = settings.MINIO_BUCKET_NAME

            if not minio_client.bucket_exists(bucket_name):
                minio_client.make_bucket(bucket_name)

            file_name = file.name
            minio_client.put_object(bucket_name, file_name, file, file.size)
            instance.file_url = f"https://{settings.MINIO_URL}/{bucket_name}/{file_name}"

        if commit:
            instance.save()
        return instance


@admin.register(Task)
class TaskAdmin(admin.ModelAdmin):
    form = TaskForm
    list_display = ['title', 'description', 'file_url', 'answer']


