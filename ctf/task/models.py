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
    correct = models.BooleanField(default=False)

    class Meta:
        unique_together = ('user', 'task')

    def __str__(self):
        return f"{self.user} - {self.task} - {'Correct' if self.correct else 'Incorrect'}"


class Profile(models.Model):
    user = models.OneToOneField(User, on_delete=models.CASCADE)
    color = models.CharField(max_length=7, default='#FFFFFF')
    postfix = models.CharField(default="", null=True)

    def __str__(self):
        return f'{self.user.username} Profile'


# class Rewards(models.Model):
#     user = models.ForeignKey(User, on_delete=models.CASCADE)
#     type = models.CharField(null=True)
#     value = models.CharField()
#
#     def __str__(self):
#         return f'{self.user.username} \n Rewards: \n\t Type: {self.type} \n\t Value: {self.value}'


# def create_profile(sender, **kwargs):
#     if kwargs['created']:
#         Profile.objects.create(user=kwargs['instance'])
#
#
# from django.db.models.signals import post_save
#
# post_save.connect(create_profile, sender=User)
