from django.db import models


# Create your models here.
class Task(models.Model):
    title = models.CharField(max_length=255, verbose_name="Заголовок")
    description = models.TextField(blank=True, verbose_name="Описание задачи")
    file_url = models.URLField(max_length=200, blank=True, null=True)
    answer = models.CharField(max_length=200, blank=True, null=True)

    def get_absolute_url(self):
        return f"/task/{self.id}/"

    def __str__(self):
        return self.title
