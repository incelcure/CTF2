import jwt
from django.conf import settings
from datetime import datetime, timedelta
from .metrics import jwt_cache_hit, jwt_cache_miss
from django.conf import settings


def get_user_token(request, username) -> str:
    if request.session.get('jwt_token', default=None):
        # if request.session["jwt_token"]:
        current_time = int(datetime.utcnow().timestamp())
        if request.session.get("jwt_expiration") > int(current_time):
            jwt_cache_hit.inc()
            return request.session.get("jwt_token")
        else:
            request.session["jwt_token"], request.session["jwt_expiration"] = generate_new_token(username)
            jwt_cache_miss.inc()
            return request.session.get("jwt_token")
    else:
        request.session["jwt_token"], request.session["jwt_expiration"] = generate_new_token(username)
        jwt_cache_miss.inc()
        return request.session.get("jwt_token")
    return None


def generate_new_token(username: str) -> str:
    payload = {
        'sub': username,
        'exp': datetime.utcnow() + timedelta(hours=1),
        'iat': datetime.utcnow()
    }
    new_token = jwt.encode(payload, settings.JWT_SECRET_KEY, algorithm=settings.JWT_ALGORITHM)
    expiration = int((datetime.utcnow() + timedelta(minutes=5)).timestamp())
    return new_token, expiration
