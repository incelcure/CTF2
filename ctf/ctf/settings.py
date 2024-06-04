"""
Django settings for ctf project.

Generated by 'django-admin startproject' using Django 5.0.4.

For more information on this file, see
https://docs.djangoproject.com/en/5.0/topics/settings/

For the full list of settings and their values, see
https://docs.djangoproject.com/en/5.0/ref/settings/
"""
import os
from pathlib import Path
from dotenv import load_dotenv
from django.contrib.messages import constants as messages
import ifaddr

load_dotenv()
# Build paths inside the project like this: BASE_DIR / 'subdir'.
BASE_DIR = Path(__file__).resolve().parent.parent

# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/5.0/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!

SECRET_KEY = os.getenv('DJANGO_SECRET_KEY')

# CASINO EXPERIENCE
CASINO_HOST = os.getenv('CASINO_HOST')
JWT_SECRET_KEY = os.getenv('DJANGO_SECRET_KEY')
JWT_ALGORITHM = 'HS256'

# USAGE: DJANGO_DEBUG='t'
DEBUG = os.getenv('DJANGO_DEBUG', str(False)).lower() in ('true', '1', 't')

# USAGE: ALLOWED_HOSTS='127.0.0.1,172.0.0.2'
allowed_hosts_str = os.getenv('ALLOWED_HOSTS', '')
ALLOWED_HOSTS = allowed_hosts_str.split(',')

unrestricted_hosts_str = os.getenv('UNRESTRICTED_HOSTS', '')
UNRESTRICTED_HOSTS = list(filter(None, unrestricted_hosts_str.split(',')))
RESTRICTED_ENDPOINTS = ['/metrics', '/admin']
UNRESTRICT_INTERFACES = os.getenv('UNRESTRICT_INTERFACES', str(False)).lower() in ('true', '1', 't')

if UNRESTRICT_INTERFACES:
    for adapter in ifaddr.get_adapters():
        for ip in adapter.ips:
            addr = ip.ip[0] if isinstance(ip.ip, tuple) else ip.ip
            UNRESTRICTED_HOSTS.append(addr)
            ALLOWED_HOSTS.append(addr)

CACHES = {
    'default': {
        'BACKEND': 'django.core.cache.backends.locmem.LocMemCache',
        'LOCATION': 'unique-snowflake',
    }
}

# MINIO envs
MINIO_URL = 's3.backyard-hg.xyz'
MINIO_ACCESS_KEY = os.getenv('MINIO_ACCESS_KEY')
MINIO_SECRET_KEY = os.getenv('MINIO_SECRET_KEY')
MINIO_BUCKET_NAME = 'ctf'
MINIO_SECURE = True  # use ssl

MINIO_USER_ACCESS_KEY = os.getenv('MINIO_USER_ACCESS_KEY')
MINIO_USER_SECRET_KEY = os.getenv('MINIO_USER_SECRET_KEY')

# CSRF Moment
csrf_trusted_origins = os.getenv('CSRF_TRUSTED_ORIGINS', '')
CSRF_TRUSTED_ORIGINS = csrf_trusted_origins.split(',')

# Application definition

INSTALLED_APPS = [
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django_prometheus',
    'task',
]

MIDDLEWARE = [
    'django_prometheus.middleware.PrometheusBeforeMiddleware',
    'django.middleware.security.SecurityMiddleware',
    'whitenoise.middleware.WhiteNoiseMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
    'task.middleware.HostFilterMiddleware',
    'task.middleware.MetricsMiddleware',
    'django_prometheus.middleware.PrometheusAfterMiddleware',
]

ROOT_URLCONF = 'ctf.urls'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [os.path.join(BASE_DIR, 'templates')],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
                'task.context_processors.logo_url',
            ],
        },
    },
]

WSGI_APPLICATION = 'ctf.wsgi.application'

# Database
# https://docs.djangoproject.com/en/5.0/ref/settings/#databases

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',
        'NAME': os.getenv('POSTGRES_DB', 'ctf'),
        'USER': os.getenv('POSTGRES_USER', 'ctf'),
        'PASSWORD': os.getenv('POSTGRES_PASSWORD'),
        'HOST': os.getenv('POSTGRES_HOST', 'localhost'),
        'PORT': os.getenv('POSTGRES_PORT', '5432'),
    }
}

# Password validation
# https://docs.djangoproject.com/en/5.0/ref/settings/#auth-password-validators

AUTH_PASSWORD_VALIDATORS = [
    {
        'NAME': 'django.contrib.auth.password_validation.UserAttributeSimilarityValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.MinimumLengthValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.CommonPasswordValidator',
    },
    {
        'NAME': 'django.contrib.auth.password_validation.NumericPasswordValidator',
    },
]

# Internationalization
# https://docs.djangoproject.com/en/5.0/topics/i18n/

LANGUAGE_CODE = 'en-us'

TIME_ZONE = 'UTC'

USE_I18N = True

USE_TZ = True

# Static files (CSS, JavaScript, Images)
# https://docs.djangoproject.com/en/5.0/howto/static-files/

STATIC_URL = 'static/'
# STATIC_ROOT = 'static'
STATIC_ROOT = os.path.join(BASE_DIR, 'static')
STATICFILES_DIRS = []

# MEDIA_ROOT = 'media'
MEDIA_ROOT = os.path.join(BASE_DIR, 'media')
MEDIA_URL = '/media/'

# Default primary key field type
# https://docs.djangoproject.com/en/5.0/ref/settings/#default-auto-field

DEFAULT_AUTO_FIELD = 'django.db.models.BigAutoField'

MESSAGE_TAGS = {
    messages.DEBUG: 'debug',
    messages.INFO: 'info',
    messages.SUCCESS: 'success',
    messages.WARNING: 'warning',
    messages.ERROR: 'error',
}
