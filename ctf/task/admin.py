from django.contrib import admin
from .models import Task


class TaskAdmin(admin.ModelAdmin):
    list_display = ('title', 'file_url', 'answer')
    search_fields = ('title', 'description')
    list_filter = ('title',)


admin.site.register(Task, TaskAdmin)
