from django.db import models
from django.contrib.auth.models import User
from .minio_client import get_presigned_url
from ctf import settings


# Create your models here.
class Task(models.Model):
    title = models.CharField(max_length=255, verbose_name="Заголовок")
    description = models.TextField(blank=True, verbose_name="Описание задачи")
    file_name = models.CharField(max_length=255, blank=True, null=True)
    # file_url = models.URLField(max_length=200, blank=True, null=True)
    answer = models.CharField(max_length=200, blank=True, null=True)
    points = models.PositiveIntegerField(default=0)

    def get_absolute_url(self):
        return f"/task/{self.id}/"

    def __str__(self):
        return self.title

    @property
    def file_url(self):
        if self.file_name:
            return get_presigned_url(self.file_name)
        return None


class Attempt(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    task = models.ForeignKey(Task, on_delete=models.CASCADE)
    timestamp = models.DateTimeField(auto_now_add=True)
    correct = models.BooleanField()

    def __str__(self):
        return f"{self.user} - {self.task} - {'Correct' if self.correct else 'Incorrect'}"
